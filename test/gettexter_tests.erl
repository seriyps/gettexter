%% @doc Uses locale/se/LC_MESSAGES/default.po
-module(gettexter_tests).

-define(GETTEXT_DOMAIN, default).
-include("shortcuts.hrl").

-include_lib("eunit/include/eunit.hrl").
-export([start_load/0, start_load_string/0]).

%% this 3 test functions shouldn't be runned concurrently, because they use
%% single gettexter server.
gettext_string_test_() ->
    {setup, fun start_load_string/0, fun stop/1,
     gettext_string_()}.

gettext_noloaded_test_() ->
    {setup, fun start/0, fun stop/1,
     {inparallel, 1, [
      dpgettext_noloaded_(),
      dnpgettext_noloaded_()
     ]}
    }.

gettext_loaded_test_() ->
    {setup, fun start_load/0, fun stop/1,
     [
      dpgettext_loaded_(),
      dnpgettext_loaded_()
     ]
    }.

gettext_string_() ->
    [?_assertEqual("Hejsan", ?_("Hello", <<"se">>)),
     ?_assertEqual("Hejsan", ?_("Hello", "se")),
     ?_assertEqual("Hejsan", ?_("Hello", se)),
     ?_assertEqual("NoTranslation", ?_("NoTranslation", "se")),
     ?_assertEqual("", ?_("", "se")),
     ?_assertEqual("Fisk", ?N_("Fish", "Fishes", 1, <<"se">>)),
     ?_assertEqual("Fisk", ?N_("Fish", "Fishes", 1, "se")),
     ?_assertEqual("Fisk", ?N_("Fish", "Fishes", 1, se)),
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
     ?_assertEqual(<<"Hejsan">>, ?_(<<"Hello">>, "se")),
     ?_assertEqual(<<"Hejsan">>, ?_(<<"Hello">>, se)),
     ?_assertEqual(<<"NoTranslation">>, ?_(<<"NoTranslation">>, <<"se">>)),
     ?_assertEqual(<<"Tjena">>, ?P_(<<"Context">>, <<"Hello">>, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?P_(<<"Context">>, <<"NoTranslation">>, <<"se">>))].

dnpgettext_noloaded_() ->
    [?_assertEqual(<<"Fish">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<"Fish">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, "se")),
     ?_assertEqual(<<"Fish">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, se)),
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

bindtextdomain_regression_test_() ->
    {setup, fun start_load/0, fun stop/1,
     [
      fun() ->
              gettexter:textdomain(?GETTEXT_DOMAIN),
              ?assertEqual(<<"Hejsan">>, gettexter:gettext(<<"Hello">>, <<"se">>))
      end,
      fun() ->
              gettexter:textdomain(?GETTEXT_DOMAIN),
              ?assertEqual("Hejsan", gettexter:gettext("Hello", <<"se">>))
      end,
      fun() ->
              gettexter:textdomain(?GETTEXT_DOMAIN),
              ?assertEqual(<<"Fisk">>, gettexter:ngettext(<<"Fish">>, <<"Fishes">>, 1, <<"se">>))
      end
     ]
    }.


start() ->
    case gettexter_server:start_link() of
         {ok, Pid} -> Pid;
         {error, {already_started, Pid}} ->
            io:format(user, "already ~p\n", [Pid]),
            Pid
    end.

ensure_mo(Domain, Dir, Locale) ->
    StrDomain = atom_to_list(Domain),
    MoFile = filename:join([Dir, Locale, "LC_MESSAGES", StrDomain ++ ".mo"]),
    case filelib:is_regular(MoFile) of
        false ->
            PoFile = filename:join([Dir, Locale, "LC_MESSAGES", StrDomain ++ ".po"]),
            Cmd = unicode:characters_to_list(["msgfmt ", "--check ", "-o ", MoFile, " ", PoFile]),
            {ok, Cwd} = file:get_cwd(),
            io:format(user, "At ~s~n~s~n~s", [Cwd, Cmd, os:cmd(Cmd)]);
        _ -> ok
    end.

start_load() ->
    Pid = start(),
    Dir = "test/locale",
    ensure_mo(?GETTEXT_DOMAIN, Dir, <<"se">>),
    ok = gettexter:bindtextdomain(?GETTEXT_DOMAIN, Dir),
    {ok, _} = gettexter:ensure_loaded(?GETTEXT_DOMAIN, lc_messages, <<"se">>),
    Pid.

start_load_string() ->
    Pid = start(),
    Dir = "test/locale",
    ensure_mo(?GETTEXT_DOMAIN, Dir, "se"),
    ok = gettexter:bindtextdomain(?GETTEXT_DOMAIN, Dir),
    {ok, _} = gettexter:ensure_loaded(?GETTEXT_DOMAIN, lc_messages, "se"),
    Pid.


stop(Pid) ->
    Prev = process_flag(trap_exit, true),
    exit(Pid, stop),
    receive {'EXIT', Pid, stop} ->
            process_flag(trap_exit, Prev)
    after 5000 ->
            error(timeout)
    end.
