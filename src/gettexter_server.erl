%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2014, Sergey Prokhorov
%%% @doc
%%% Locale information storage. Operates only binary data (except `domain').
%%% @end
%%% Created : 25 Feb 2014 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([dpgettext/4, dnpgettext/6]).
-export([bindtextdomain/2]).
-export([ensure_loaded/3, which_domains/1, which_locales/1, which_loaded/0, which_keys/2, header/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -export([to_lower/1, strip/1]).
-define(SERVER, ?MODULE).
-define(TAB, gettexter_server_ets).

%% ETS key types
-define(MSG_KEY(Domain, Locale, Context, Text),
        ?PLURAL_MSG_KEY(Domain, Locale, Context, Text, undefined, 0)).
-define(PLURAL_MSG_KEY(Domain, Locale, Context, Singular, Plural, Form),
        {msg, Domain, Locale, Context, Singular, Plural, Form}).
-define(PLURAL_RULE_KEY(Domain, Locale), {plural_rule, Domain, Locale}).
-define(HEADER_KEY(Domain, Locale, Name), {hdr, Domain, Locale, Name}).
-define(LOADED_KEY(Domain, Locale), {loaded, Domain, Locale}).
-define(BINDING_KEY(Domain), {binding, Domain}).

-record(state, {tab}).

%% API Function Definitions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec dpgettext(atom(), binary(), binary(), binary()) -> binary().
dpgettext(Domain, Context, Text, Locale) ->
    case ets:lookup(?TAB, ?MSG_KEY(Domain, Locale, Context, Text)) of
        []                 -> undefined;
        [{_, Translation}] -> Translation
    end.

-spec dnpgettext(atom(), binary(), binary(), binary(), binary(), integer()) -> binary().
dnpgettext(Domain, Context, Singular, Plural, N, Locale) ->
    case ets:lookup(?TAB, ?PLURAL_RULE_KEY(Domain, Locale)) of
        []                  -> undefined;
        [{_, CompiledRule}] ->
            Form = gettexter_plural:plural(N, CompiledRule),
            PluralKey = ?PLURAL_MSG_KEY(Domain, Locale, Context, Singular,
                                        Plural, Form),
            case ets:lookup(?TAB, PluralKey) of
                []                 -> undefined;
                [{_, Translation}] -> Translation
            end
    end.

-spec bindtextdomain(atom(), file:filename()) -> ok.
bindtextdomain(Domain, LocaleDir) ->
    gen_server:call(?SERVER, {bindtextdomain, Domain, LocaleDir}).

-spec ensure_loaded(atom(), atom(), binary()) ->
                           {ok, already | file:filename()}
                               | {error, any()}.
ensure_loaded(TextDomain, _Category, Locale) ->
    case ets:member(?TAB, ?LOADED_KEY(TextDomain, Locale)) of
        true  -> {ok, already};
        false -> gen_server:call(?SERVER, {ensure_loaded, TextDomain, Locale})
    end.

-spec which_domains(binary()) -> [atom()].
which_domains(Locale) ->
    [Domain || [Domain] <- ets:match(?TAB, {?LOADED_KEY('$1', Locale), '_'})].

-spec which_locales(atom()) -> [binary()].
which_locales(Domain) ->
    [Locale || [Locale] <- ets:match(?TAB, {?LOADED_KEY(Domain, '$1'), '_'})].

-spec which_loaded() -> [{Domain, Locale, MoPath}]
                            when
      Domain :: atom(),
      Locale :: binary(),
      MoPath :: file:filename().
which_loaded() ->
    [list_to_tuple(L) || L <- ets:match(?TAB, {?LOADED_KEY('$1', '$2'), '$3'})].

-spec which_keys(atom(), binary()) -> [{Singular, Plural, Context}]
                                          when
      Context :: undefined | binary(),
      Singular :: binary(),
      Plural :: binary().
which_keys(Domain, Locale) ->
    lists:usort(
      [list_to_tuple(L)
       || L <- ets:match(
                 ?TAB,
                 {?PLURAL_MSG_KEY(Domain, Locale, '$3', '$1', '$2', '_'), '_'})]).

-spec header(atom(), binary(), binary()) -> undefined | binary().
header(Domain, Locale, Name) ->
    case ets:lookup(?TAB, ?HEADER_KEY(Domain, Locale, Name)) of
        []           -> undefined;
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
                    catch Type:Reason:Trace ->
                            %% cleanup possible partial load
                            catch unload_locale(?TAB, Domain, Locale),
                            {error, {Type, Reason, Trace}}
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
    Binding = case ets:lookup(Tab, ?BINDING_KEY(Domain)) of
                  [] ->
                      rel_to_abs_path(Domain, "locale");
                  [{_, AbsPath = "/" ++ _}] ->
                      AbsPath;
                  [{_, RelPath}] ->
                      rel_to_abs_path(Domain, RelPath)
              end,
    AbsBinding = filename:absname(Binding),
    MoFileName = filename:join([AbsBinding, Locale, "LC_MESSAGES", atom_to_list(Domain) ++ ".mo"]),
    %% extract messages from .mo
    Parsed = gettexter_mo_parser:parse_file(MoFileName),
    Catalog = gettexter_mo_parser:to_dict(Parsed),
    %% fill ETS table
    Headers = lists:foldl(
                fun({{<<"">>, undefined, _}, [MimeData]}, undefined) ->
                        parse_headers(MimeData);
                   ({{Singular, Plural, Context}, Translations}, MD) ->
                        lists:foldl(
                          fun(Translation, Form) ->
                                  Key = ?PLURAL_MSG_KEY(Domain, Locale, Context,
                                                        Singular, Plural, Form),
                                  true = ets:insert(Tab, {Key, Translation}),
                                  Form + 1
                          end,
                          0, Translations
                         ),
                        MD
                end,
                undefined, Catalog
               ),
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

%% The idea is that usualy Domain is named as your OTP application, so, if you
%% have 2 apps: one is your main app and the second one is dependency, FS layout
%% will be
%%
%% my_main
%%     src
%%         my_main_app.erl
%%     priv
%%         locale
%%             en
%%               LC_MESSAGES
%%                 my_main.mo
%%             ru
%%               ...
%%     deps
%%         my_dep
%%             src
%%                 my_dep_app.erl
%%             priv
%%                 locale
%%                     en
%%                         LC_MESSAGES
%%                             my_dep.mo
%%                     ru
%% And my_dep_app.erl will have lines like gettexter:textdomain(my_dep), while
%% my_main_app.erl will use `gettexter:textdomain(my_main)'.
%%
%% So, with this filename:join(code:priv_dir(Domain), "locale") each app will
%% load locales from it's own locale directory - both apps may use single
%% gettexter server without any conflicts.
rel_to_abs_path(Domain, RelPath) ->
    BaseDir = case code:priv_dir(Domain) of
                  {error, bad_name} ->
                      %% domain isn't the name of loaded application. Try to
                      %% load locale from current dir
                      {ok, Cwd} = file:get_cwd(),
                      Cwd;
                  PrivDir ->
                      %% domain is the name of some application. So, we use
                      %% application's priv directory as base dir
                      PrivDir
              end,
    filename:join(BaseDir, RelPath).

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
    true = ets:match_delete(Tab, {?PLURAL_MSG_KEY(Domain, Locale, '_', '_', '_', '_'), '_'}),
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
