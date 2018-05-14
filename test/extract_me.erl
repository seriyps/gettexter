-module(extract_me).

%% gettext
-export([functions/0,
         multi_bin/0,
         comments/0]).

functions() ->
    %% gettext/1,2
    gettexter:gettext("gettext1"),
    gettexter:gettext("gettext2", "se"),

    % pgettext/2,3
    gettexter:pgettext("pgettext", "pgettext1"),
    gettexter:pgettext("pgettext", "pgettext2", "se"),

    % dgettext/2,3
    gettexter:dgettext(dgettext, "dgettext1"),
    gettexter:dgettext(dgettext, "dgettext2", "se"),

    % dpgettext/3,4
    gettexter:dpgettext(dpgettext, "dpgettext", "dpgettext1"),
    gettexter:dpgettext(dpgettext, "dpgettext", "dpgettext2", "se"),

    % ngettext/3,4
    gettexter:ngettext("ngettext1", "ngettexts", 5),
    gettexter:ngettext("ngettext2", "ngettexts", 5, "se"),

    % npgettext/4,5
    gettexter:npgettext("npgettext", "npgettext1", "npgettexts"),
    gettexter:npgettext("npgettext", "npgettext2", "npgettexts", "se"),

    % dngettext/4,5
    gettexter:dngettext(dngettext, "dngettext1", "dngettexts", 5),
    gettexter:dngettext(dngettext, "dngettext2", "dngettexts", 5, "se"),

    % dnpgettext/5,6
    gettexter:dnpgettext(dnpgettext, "dnpgettext", "dnpgettext1", "dnpgettexts", 5),
    gettexter:dnpgettext(dnpgettext, "dnpgettext", "dnpgettext2", "dnpgettexts", 5, "se").

multi_bin() ->
    gettexter:gettext(<<"m", "u", "l", "t", "i", "binary">>).

comments() ->
    %% Test:
    %% Comment for the translator
    gettexter:gettext("comment1"),

    %% Wrong:
    %% This is not a comment for the translator
    gettexter:gettext("comment2"),

    %% Test:
    %% Multi
    %% Line
    gettexter:gettext("comment3").
