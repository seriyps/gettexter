-module(translate_me).
-export([main/3]).

-define(GETTEXT_DOMAIN, my_app).
-include("../include/shortcuts.hrl").
-define(OD, other_app).

main(Name, What, N) ->
    gettexter:bindtextdomain(?GETTEXT_DOMAIN, "priv/locales"),
    gettexter:setlocale(undefined, "ru"),
    gettexter:textdomain(?GETTEXT_DOMAIN),

    Question = case What of
                 sleep -> ?NO_("Wanna sleep?");
                 eat -> ?NO_(<<"Wanna eat?">>)
               end,
    Suggest = case What of
                  sleep -> ?N_("go sleep a hour",
                               "go sleep ~p hours", N);
                  eat -> ?N_("take one babana",
                             "take ~p bananas", N)
              end,
    %% /* Translators: this is hello message */
    io:format(?_("Hello, ~p! ~ts"), [Name, ?_(Question)]),
    io:format(?_("So, you may ~ts"), [Suggest]),
    io:format(?D_(?OD, "Other domain")),
    io:format(?DN_(?OD, "One other domain", "~p other domains", N)),
    io:format(?NP_("the-context", "One other domain", "~p other domains", N)).
