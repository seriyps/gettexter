%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2014, Sergey Prokhorov
%%% @doc
%%% Locale information storage.
%%% @end
%%% Created : 25 Feb 2014 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([dpgettext/4, dnpgettext/6]).
-export([bindtextdomain/2]).
-export([ensure_loaded/3, which_domains/1, which_locales/1, which_loaded/0, header/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -export([to_lower/1, strip/1]).
-define(SERVER, ?MODULE).
-define(TAB, gettexter_server_ets).

%% ETS key types
-define(MSG_KEY(Domain, Locale, Context, Msgid),
        ?PLURAL_MSG_KEY(Domain, Locale, Context, Msgid, undefined)).
-define(PLURAL_MSG_KEY(Domain, Locale, Context, Singular, Plural),
        {msg, Domain, Locale, Context, Singular, Plural}). %XXX: maybe store each plural form in separate object? More RAM, but faster retrieval.
-define(PLURAL_RULE_KEY(Domain, Locale), {plural_rule, Domain, Locale}).
-define(HEADER_KEY(Domain, Locale, Name), {hdr, Domain, Locale, Name}).
-define(LOADED_KEY(Domain, Locale), {loaded, Domain, Locale}).
-define(BINDING_KEY(Domain), {binding, Domain}).

-record(state, {tab}).

%% API Function Definitions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

dpgettext(Domain, Context, Locale, Msgid) ->
    case ets:lookup(?TAB, ?MSG_KEY(Domain, Locale, Context, Msgid)) of
        [] -> undefined;
        [{_, [Msgstr]}] -> Msgstr
    end.

dnpgettext(Domain, Context, Locale, Singular, Plural, N) ->
    case ets:lookup(?TAB, ?PLURAL_RULE_KEY(Domain, Locale)) of
        [] -> undefined;
        [{_, CompiledRule}] ->
            case ets:lookup(?TAB, ?PLURAL_MSG_KEY(Domain, Locale, Context, Singular, Plural)) of
                [] -> undefined;
                [{_, Messages}] ->
                    Form = gettexter_plural:plural(N, CompiledRule),
                    lists:nth(Form + 1, Messages)
            end
    end.

bindtextdomain(Domain, LocaleDir) ->
    gen_server:call(?SERVER, {bindtextdomain, Domain, LocaleDir}).


ensure_loaded(TextDomain, _Category, Locale) ->
    case ets:member(?TAB, ?LOADED_KEY(TextDomain, Locale)) of
        true -> {ok, already};
        false ->
            gen_server:call(?SERVER, {ensure_loaded, TextDomain, Locale})
    end.

which_domains(Locale) ->
    [Domain || [Domain] <- ets:match(?TAB, {?LOADED_KEY('$1', Locale), '_'})].

which_locales(Domain) ->
    [Locale || [Locale] <- ets:match(?TAB, {?LOADED_KEY(Domain, '$1'), '_'})].

which_loaded() ->
    [list_to_tuple(L) || L <- ets:match(?TAB, {?LOADED_KEY('$1', '$2'), '$3'})].

header(Domain, Locale, Name) ->
    case ets:lookup(?TAB, ?HEADER_KEY(Domain, Locale, Name)) of
        [] -> undefined;
        [{_, Value}] -> Value
    end.

%% gen_server Function Definitions
init([]) ->
    Tid = ets:new(?TAB, [named_table,
                         protected,
                         set,
                         {keypos, 1},
                         {read_concurrency, true}]),
    {ok, #state{tab=Tid}}.

handle_call({bindtextdomain, Domain, LocaleDir}, _From, State) ->
    true = ets:insert(?TAB, {?BINDING_KEY(Domain), LocaleDir}),
    {reply, ok, State};
handle_call({ensure_loaded, Domain, Locale}, _From, State) ->
    Reply = case ets:member(?TAB, ?LOADED_KEY(Domain, Locale)) of
                true -> {ok, already};
                false ->
                    try
                        load_locale(?TAB, Domain, Locale)
                    catch Type:Reason ->
                            catch unload_locale(?TAB, Domain, Locale), % cleanup possible partial load
                            {error, {Type, Reason, erlang:get_stacktrace()}}
                    end
            end,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

load_locale(Tab, Domain, Locale) ->
    %% code:lib_dir/1 may return {error, ...}, so this should be wrapped by try...catch
    %% calculate path to .mo file
    Binding = case ets:lookup(Tab, ?BINDING_KEY(Domain)) of
                  [] ->
                      filename:join(code:lib_dir(Domain), "locale");
                  [{_, AbsPath = "/" ++ _}] ->
                      AbsPath;
                  [{_, RelPath}] ->
                        filename:join(code:lib_dir(Domain), RelPath)
                 end,
    AbsBinding = filename:absname(Binding),
    MoFileName = filename:join([AbsBinding, Locale, "LC_MESSAGES", atom_to_list(Domain) ++ ".mo"]),
    %% extract messages from .mo
    Parsed = gettexter_mo_parser:parse_file(MoFileName),
    Catalog = gettexter_mo_parser:to_dict(Parsed),
    %% fill ETS table
    B2LIfNotNull = fun(undefined) -> undefined;
                      (V) -> binary_to_list(V)
                   end,
    Headers = lists:foldl(fun({{<<"">>, undefined, _}, [MimeData]}, undefined) ->
                                  parse_headers(MimeData);
                             ({{Singular, Plural, Context}, Values}, MD) ->
                                  Ob = {?PLURAL_MSG_KEY(Domain, Locale,
                                                        B2LIfNotNull(Context),
                                                        B2LIfNotNull(Singular),
                                                        B2LIfNotNull(Plural)),
                                        lists:map(fun erlang:binary_to_list/1, Values)},
                                  true = ets:insert(Tab, Ob),
                                  MD
                          end, undefined, Catalog),
    case Headers of
        {ok, HeadersList} ->
            load_plural_rule(Tab, Domain, Locale, HeadersList),
            HObjs = lists:map(fun({K, V}) ->
                                      {?HEADER_KEY(Domain, Locale, K), V}
                              end, HeadersList),
            true = ets:insert(Tab, HObjs);
        Other ->
            error_logger:warning_msg("Locale metadata for locale '~ts' domain ~p"
                                     " not available: ~p", [Locale, Domain, Other])
    end,
    true = ets:insert(Tab, {?LOADED_KEY(Domain, Locale), MoFileName}),
    {ok, MoFileName}.

load_plural_rule(Tab, Domain, Locale, Headers) ->
    case proplists:get_value(<<"plural-forms">>, Headers) of
        undefined ->
            error_logger:warning_msg(
              "Plural-Forms for locale '~ts' domain ~p not available",
              [Locale, Domain]);
        ValueBin ->
            PluralRule = gettexter_plural:compile(binary_to_list(ValueBin), []),
            true = ets:insert(Tab, {?PLURAL_RULE_KEY(Domain, Locale), PluralRule})
    end.

unload_locale(Tab, Domain, Locale) ->
    true = ets:match_delete(Tab, {?PLURAL_MSG_KEY(Domain, Locale, '_', '_', '_'), '_'}),
    true = ets:match_delete(Tab, {?HEADER_KEY(Domain, Locale, '_'), '_'}),
    true = ets:match_delete(Tab, {?PLURAL_RULE_KEY(Domain, Locale), '_'}),
    true = ets:match_delete(Tab, {?LOADED_KEY(Domain, Locale), '_'}),
    ok.

parse_headers(MimeData) ->
    %% Super-simple 'K: V\n' parser
    Lines = binary:split(MimeData, <<"\n">>, [global, trim]),
    Hdrs = lists:map(fun(Line) ->
                             [K, V] = binary:split(Line, <<":">>),
                             {to_lower(strip(K)), strip(V)}
                     end,
                     Lines),
    {ok, Hdrs}.


-define(IS_WSP(C), ($\ == C) orelse ($\r == C) orelse ($\n == C) orelse ($\t == C)).

strip(<<C/integer, Rest/binary>>) when ?IS_WSP(C) ->
    strip(Rest);
strip(Str) ->
    Pos = size(Str) - 1,
    case Str of
        <<Rest:Pos/binary, C>> when ?IS_WSP(C) ->
            strip(Rest);
        Other ->
            Other
    end.

to_lower(Str) ->
    << <<(case (C >= $A) and (C =< $Z ) of
              true -> C - $A + $a;
              false -> C
          end)>> || <<C>> <= Str>>.
