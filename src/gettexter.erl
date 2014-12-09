%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @author Emil Falk <emil.falk@textalk.se>
%% @copyright (C) 2014, Sergey Prokhorov
%% @copyright (C) 2014, Emil Falk
%% @doc
%% == gettexter ==
%%
%% The gettexter API exports functions similar to those offered by GNU gettext.
%% All lookup function works with both binaries and strings but not mixed. The
%% locale may be either atom, binary or string.
%%
%% == Macros ==
%% Several macros can be found in `include/shortcuts.hrl'. They are provided as
%% a convienient way to mark up your strings or binaries and to simplify
%% extraction via xgettext. All possible combinations from the API is provided.
%% To set a default domain for the macros with implicit domain below use the
%% constant GETTEXT_DOMAIN as for example:
%%
%% `-define(GETTEXT_DOMAIN, users).'
%%
%% === Locale in process dictionary ===
%%
%% ==== Domain from GETTEXT_DOMAIN ====
%% <ul>
%%     <li>?_(Text)</li>
%%     <li>?N_(Singular, Plural, N)</li>
%%     <li>?P_(Context, Text)</li>
%%     <li>?NP_(Context, Singular, Plural, N)</li>
%% </ul>
%%
%% ==== Domain as argument ====
%% <ul>
%%     <li>?D_(Domain, Text)</li>
%%     <li>?DN_(Domain, Singular, Plural, N)</li>
%%     <li>?DP_(Domain, Context)</li>
%%     <li>?DNP_(Domain, Context, Singular, Plural, N)</li>
%% </ul>
%%
%% === Locale as argument ===
%%
%% ==== Domain from GETTEXT_DOMAIN ====
%% <ul>
%%     <li>?_(Text, Locale)</li>
%%     <li>?N_(Singular, Plural, N, Locale)</li>
%%     <li>?P_(Context, Text, Locale)</li>
%%     <li>?NP_(Context, Singular, Plural, N, Locale)</li>
%% </ul>
%%
%% ==== Domain as argument ====
%% <ul>
%%     <li>?D_(Domain, Text, Locale)</li>
%%     <li>?DN_(Domain, Singular, Plural, N, Locale)</li>
%%     <li>?DP_(Domain, Context, Locale)</li>
%%     <li>?DNP_(Domain, Context, Singular, Plural, N, Locale)</li>
%% </ul>
%% @end
%% Created : 25 Feb 2014 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter).

%% gettext
-export([gettext/1, gettext/2,
         pgettext/2, pgettext/3,
         dgettext/2, dgettext/3,
         dpgettext/3, dpgettext/4,
         ngettext/3, ngettext/4,
         npgettext/4, npgettext/5,
         dngettext/4, dngettext/5,
         dnpgettext/5, dnpgettext/6]).

%% configuration
-export([bindtextdomain/2]).
-export([setlocale/2, getlocale/1]).
-export([textdomain/0, textdomain/1]).
-export([bind_textdomain_codeset/2]).

%% auxillary
-export([which_domains/1, which_locales/1, ensure_loaded/3, reset/0]).

%% helper types
-type text() :: binary() | string().
-type locale() :: atom() | binary() | string().
-export_type([text/0, locale/0]).

%%
%% gettext
%%

%% @doc Translate text.
%% Locale in process dictionary.
-spec gettext(Text :: Type) -> Type when Type :: text().
gettext(Text) -> dpgettext(undefined, undefined, Text).

%% @doc Translate text to a given locale
-spec gettext(Text :: Type, Locale :: locale()) -> Type when Type :: text().
gettext(Text, Locale) -> dpgettext(undefined, undefined, Text, Locale).

%% @doc Translate plural text.
-spec ngettext(Singular :: Type, Plural :: Type, N :: integer()) ->
    Type when Type :: text().
ngettext(Singular, Plural, N) ->
    dnpgettext(undefined, undefined, Singular, Plural, N).

%% @doc Translate plural text given a locale.
-spec ngettext(Singular :: Type, Plural :: Type, N :: integer(),
               Locale :: locale()) -> Type when Type :: text().
ngettext(Singular, Plural, N, Locale) ->
    dnpgettext(undefined, undefined, Singular, Plural, N, Locale).

%% @doc Translate a text in a context.
-spec pgettext(Context :: Type | undefined, Text :: Type) ->
    Type when Type :: text().
pgettext(Context, Text) -> dpgettext(undefined, Context, Text).

%% @doc Translate a text in a context given a locale.
-spec pgettext(Context :: Type | undefined, Text :: Type, Locale :: locale()) ->
    Type when Type :: text().
pgettext(Context, Text, Locale) -> dpgettext(undefined, Context, Text, Locale).

%% @doc Translate a plural text in a context.
-spec npgettext(Context  :: Type | undefined, Singular :: Type,
                Plural :: Type, N :: integer()) -> Type when Type :: text().
npgettext(Context, Singular, Plural, N) ->
    dnpgettext(undefined, Context, Singular, Plural, N).

%% @doc Translate a plural text in a context given a locale.
-spec npgettext(Context  :: Type | undefined, Singular :: Type,
                Plural :: Type, N :: integer(), Locale :: locale()) ->
    Type when Type :: text().
npgettext(Context, Singular, Plural, N, Locale) ->
    dnpgettext(undefined, Context, Singular, Plural, N, Locale).

%% @doc Translate a domain-specific text.
-spec dgettext(Domain :: atom(), Text :: Type) -> Type when Type :: text().
dgettext(Domain, Text) -> dpgettext(Domain, undefined, Text).

%% @doc Translate a domain-specific text given a locale.
-spec dgettext(Domain :: atom(), Text :: Type, Locale :: locale()) ->
    Type when Type :: text().
dgettext(Domain, Text, Locale) -> dpgettext(Domain, undefined, Text, Locale).

%% @doc Translate a domain-specific plural text.
-spec dngettext(Domain :: atom(), Singular :: Type, Plural :: Type,
                N :: integer()) -> Type when Type :: text().
dngettext(Domain, Singular, Plural, N) ->
    dnpgettext(Domain, undefined, Singular, Plural, N).

%% @doc Translate a domain-specific plural text given a locale.
-spec dngettext(Domain :: atom(), Singular :: Type, Plural :: Type,
                N :: integer(), Locale :: locale()) -> Type when Type :: text().
dngettext(Domain, Singular, Plural, N, Locale) ->
    dnpgettext(Domain, undefined, Singular, Plural, N, Locale).

%% @doc Translate a domain-specific text in a context.
-spec dpgettext(Domain :: atom(), Context :: Type | undefined,
                Text :: Type) -> Type when Type :: text().
dpgettext(Domain, Context, Text) ->
    Locale = getlocale(lc_messages),
    Domain1 = if Domain == undefined -> textdomain();
                 true -> Domain
              end,
    dpgettext(Domain1, Context, Text, Locale).

%% @doc Translate a domain-specific text in a context given a locale.
-spec dpgettext(Domain :: atom(), Context :: Type | undefined,
                Text :: Type, Locale :: locale()) -> Type when Type :: text().
%% binary case
dpgettext(Domain, Context, Text, Locale)
  when (Context == undefined andalso is_binary(Text)) orelse
       is_binary(Context) ->
    case gettexter_server:dpgettext(Domain, Locale, Context, Text) of
        undefined   -> Text;
        Translation -> Translation
    end;
%% string case - undefined context
dpgettext(Domain, undefined, StrText, Locale) ->
    Text = unicode:characters_to_binary(StrText),
    Translation = dpgettext(Domain, undefined, Text, Locale),
    unicode:binary_to_characters(Translation);
%% string case
dpgettext(Domain, StrContext, StrText, Locale) ->
    ToString = fun unicode:characters_to_binary/1,
    [Context, Text] = lists:map(ToString, [StrContext, StrText]),
    Translation = dpgettext(Domain, Context, Text, Locale),
    unicode:binary_to_characters(Translation).

%% @doc Translate a domain-specific plural text in a context.
-spec dnpgettext(Domain :: atom(), Context :: Type | undefined,
                 Singular :: Type, Plural :: Type, N :: integer()) ->
    Type when Type :: text().
dnpgettext(Domain, Context, Singular, Plural, N) ->
    Locale = getlocale(lc_messages),
    Domain1 = if Domain == undefined -> textdomain();
                 true -> Domain
              end,
    dnpgettext(Domain1, Context, Singular, Plural, N, Locale).

%% @doc Translate a domain-specific plural text in a context given a locale.
-spec dnpgettext(Domain :: atom(), Context :: Type | undefined,
                 Singular :: Type, Plural :: Type, N :: integer(),
                 Locale :: locale()) -> Type when Type :: text().
dnpgettext(Domain, Context, Singular, Plural, N, Locale) ->
    Translation = gettexter_server:dnpgettext(Domain, Context, Singular, Plural,
                                              N, Locale),
    case Translation of
        undefined when N == 1 -> Singular;
        undefined             -> Plural;
        Translation           -> Translation
    end.

%%
%% configuration
%%

%% @doc Bind a domain to a directory.
%% Bind a given domain to a directory where the compiled translated
%% .mo-files are located. Should be called att configuration time,
%% otherwise the default directory priv/locale/ will be used.
-spec bindtextdomain(Domain :: atom(), Localedir :: file:filename()) -> ok.
bindtextdomain(Domain, LocaleDir) ->
    gettexter_server:bindtextdomain(Domain, LocaleDir).

%% @doc Set the locale in the process dictionary.
-spec setlocale(Category :: lc_messages, Locale :: locale()) -> ok.
setlocale(Category=lc_messages, Locale) ->
    TextDomain = textdomain(),

    % assert
    true = (TextDomain =/= undefined), 

    put({?MODULE, locale, TextDomain, Category}, Locale),
    {ok, _} = gettexter_server:ensure_loaded(TextDomain, Category, Locale),
    ok.

%% @doc Get the locale from the process dictionary.
-spec getlocale(Category :: lc_messages) -> locale() | undefined.
getlocale(Category=lc_messages) ->
    TextDomain = textdomain(),
    get({?MODULE, locale, TextDomain, Category}).

%% @doc Set the text domain.
-spec textdomain(Domain :: atom()) -> ok.
textdomain(Domain) ->
    put({?MODULE, textdomain}, Domain).

%% @doc Get the text domain.
-spec textdomain() -> atom() | undefined.
textdomain() ->
    get({?MODULE, textdomain}).

%% @todo
%% @doc TODO
-spec bind_textdomain_codeset(atom(), string()) -> ok.
bind_textdomain_codeset(_Domain, _Charset) ->
    error(not_implemented).

%%
%% auxillary
%%

%% @doc Which domains are loaded for `Locale'.
-spec which_domains(Locale :: locale()) -> [atom()].
which_domains(Locale) ->
    gettexter_server:which_domains(Locale).

%% @doc Which locales are loaded for `Domain'.
-spec which_locales(Domain :: atom()) -> [locale()].
which_locales(Domain) ->
    gettexter_server:which_locales(Domain).

%% @doc Ensure that `Locale' is loaded.
%% If the locale isn't loaded, all lookups will return their own inputs.
-spec ensure_loaded(Domain :: atom(), Category :: lc_messages,
                    Locale :: locale()) ->
    {ok, file:filename() | already} |{error, any()}.
ensure_loaded(Domain, Category, Locale) ->
    gettexter_server:ensure_loaded(Domain, Category, Locale).

%% @doc Reset the process dictionary
%% Remove all gettext stuff from process dictionary (but not from locale data
%% storage).
-spec reset() -> ok.
reset() ->
    lists:foreach(fun({Key, _}) when element(1, Key) == ?MODULE -> erase(Key);
                     (_) -> ok
                  end,
                  get()),
    ok.
