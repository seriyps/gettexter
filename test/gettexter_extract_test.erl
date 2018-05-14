-module(gettexter_extract_test).

-include_lib("eunit/include/eunit.hrl").

extract_test_() ->
    {setup, fun extract/0, fun cleanup/1,
     fun (DumpTab) ->
         [
          extract_test(DumpTab),
          multi_bin_test(DumpTab),
          comments_test(DumpTab)
         ]
     end
    }.

extract_test(DumpTab) ->
    fun () ->
        %% gettext/1,2
        ?assert(ets:member(DumpTab, {undefined, undefined, "gettext1", undefined})),
        ?assert(ets:member(DumpTab, {undefined, undefined, "gettext2", undefined})),

        % pgettext/2,3
        ?assert(ets:member(DumpTab, {undefined, "pgettext", "pgettext1", undefined})),
        ?assert(ets:member(DumpTab, {undefined, "pgettext", "pgettext2", undefined})),

        % dgettext/2,3
        ?assert(ets:member(DumpTab, {dgettext, undefined, "dgettext1", undefined})),
        ?assert(ets:member(DumpTab, {dgettext, undefined, "dgettext2", undefined})),

        % dpgettext/3,4
        ?assert(ets:member(DumpTab, {dpgettext, "dpgettext", "dpgettext1", undefined})),
        ?assert(ets:member(DumpTab, {dpgettext, "dpgettext", "dpgettext2", undefined})),

        % ngettext/3,4
        ?assert(ets:member(DumpTab, {undefined, undefined, "ngettext1", "ngettexts"})),
        ?assert(ets:member(DumpTab, {undefined, undefined, "ngettext2", "ngettexts"})),

        % npgettext/4,5
        ?assert(ets:member(DumpTab, {undefined, "npgettext", "npgettext1", "npgettexts"})),
        ?assert(ets:member(DumpTab, {undefined, "npgettext", "npgettext2", "npgettexts"})),

        % dngettext/4,5
        ?assert(ets:member(DumpTab, {dngettext, undefined, "dngettext1", "dngettexts"})),
        ?assert(ets:member(DumpTab, {dngettext, undefined, "dngettext2", "dngettexts"})),

        % dnpgettext/5,6
        ?assert(ets:member(DumpTab, {dnpgettext, "dnpgettext", "dnpgettext1", "dnpgettexts"})),
        ?assert(ets:member(DumpTab, {dnpgettext, "dnpgettext", "dnpgettext2", "dnpgettexts"}))
    end.

multi_bin_test(DumpTab) ->
    fun () ->
        %% Check that multi-part binaries are extracted
        ?assert(ets:member(DumpTab, {undefined, undefined, "multibinary", undefined}))
    end.


comments_test(DumpTab) ->
    fun () ->
        %% Check that the custom tag extraction works
        ?assertMatch([{_, [{_, _, ["Comment for the translator"]}]}],
                     ets:lookup(DumpTab, {undefined, undefined, "comment1", undefined})),

        ?assertMatch([{_, [{_, _, undefined}]}],
                     ets:lookup(DumpTab, {undefined, undefined, "comment2", undefined})),

        ?assertMatch([{_, [{_, _, ["Multi", "Line"]}]}],
                     ets:lookup(DumpTab, {undefined, undefined, "comment3", undefined}))
    end.

extract() ->
    Options = [{comment_tag, "Test:"}],
    gettexter_extract:extract_to_ets([extract_me], Options).

cleanup(DumpTab) ->
    ets:delete(DumpTab).
