%% @doc Uses locale/se/LC_MESSAGES/default.po
-module(gettexter_test).

-include("shortcuts.hrl").

-include_lib("eunit/include/eunit.hrl").

gettext_string_test_() ->
    {setup, fun start_load_string/0, fun stop/1,
     [fun gettext_string_/0]}.

gettext_noloaded_test_() ->
    {setup, fun start/0, fun stop/1,
     [
      fun dpgettext_noloaded_/0,
      fun dnpgettext_noloaded_/0
     ]
    }.

gettext_loaded_test_() ->
    {setup, fun start_load/0, fun stop/1,
     [
      fun dpgettext_loaded_/0,
      fun dnpgettext_loaded_/0
     ]
    }.

gettext_string_() ->
    [?_assertEqual("Hejsan", ?_("Hello", "se")),
     ?_assertEqual("NoTranslation", ?_("NoTranslation", "se")),
     ?_assertEqual("", ?_("", "se")),
     ?_assertEqual("Fisk", ?N_("Fish", "Fishes", 1, "se")),
     ?_assertError(function_clause, ?P_(<<"BinaryContext">>, "StringText", "se")),
     ?_assertError(function_clause, ?NP_(<<"BinaryContext">>, "Singular", "Plural", "se"))].

dpgettext_noloaded_() ->
     [?_assertEqual(<<"Hello">>, ?_(<<"Hello">>, <<"se">>)),
      ?_assertEqual(<<>>, ?_(<<>>, <<"se">>)),
      ?_assertEqual(<<"Hello">>, ?P_(<<"Context">>, <<"Hello">>, <<"se">>)),
      ?_assertEqual(<<>>, ?P_(<<>>, <<>>, <<"se">>)),
      ?_assertEqual(<<>>, ?_(<<>>, <<>>)),
      ?_assertEqual(<<>>, ?P_(<<>>, <<>>, <<>>))].

dpgettext_loaded_() ->
    [?_assertEqual(<<"Hejsan">>, ?_(<<"Hello">>, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?_(<<"NoTranslation">>, <<"se">>)),
     ?_assertEqual(<<"Tjena">>, ?P_(<<"Context">>, <<"Hello">>, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?P_(<<"Context">>, <<"NoTranslation">>, <<"se">>))].

dnpgettext_noloaded_() ->
    [?_assertEqual(<<"Fish">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<"Fishes">>, ?N_(<<"Fish">>, <<"Fishes">>, 2, <<"se">>)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<>>, ?N_(<<"Fish">>, <<>>, 2, <<"se">>)),
     ?_assertEqual(<<"Goat">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 1, <<"se">>)),
     ?_assertEqual(<<"Goats">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 2, <<"se">>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<"Goats">>, 1, <<"se">>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<"Goat">>, <<>>, 2, <<"se">>)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<>>, 1, <<"se">>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<>>, 1, <<"se">>))].

dnpgettext_loaded_() ->
    [?_assertEqual(<<"Fisk">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<"Fiskar">>, ?N_(<<"Fish">>, <<"Fishes">>, 2, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?N_(<<"NoTranslation">>, <<"NoTranslations">>, 1, <<"se">>)),
     ?_assertEqual(<<"NoTranslations">>, ?N_(<<"NoTranslation">>, <<"NoTranslations">>, 2, <<"se">>)),
     ?_assertEqual(<<"Get">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 1, <<"se">>)),
     ?_assertEqual(<<"Getter">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 2, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?NP_(<<"Context">>, <<"NoTranslation">>, <<"NoTranslations">>, 1, <<"se">>)),
     ?_assertEqual(<<"NoTranslations">>, ?NP_(<<"Context">>, <<"NoTranslation">>, <<"NoTranslations">>, 2, <<"se">>))].

start() ->
    case gettexter_server:start_link() of
         {ok, Pid} -> Pid;
         {error, {already_started, Pid}} -> Pid
    end.

start_load() ->
    Pid = start(),
    gettexter:bindtextdomain(?GETTEXT_DOMAIN, "../test/locale"),
    gettexter:ensure_loaded(?GETTEXT_DOMAIN, lc_messages, <<"se">>),
    Pid.

start_load_string() ->
    Pid = start(),
    gettexter:bindtextdomain(?GETTEXT_DOMAIN, "../test/locale"),
    gettexter:ensure_loaded(?GETTEXT_DOMAIN, lc_messages, "se"),
    Pid.


stop(Pid) ->
    exit(Pid, normal).
