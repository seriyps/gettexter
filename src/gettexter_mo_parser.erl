%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%% Simple low-level gettext .mo file format parser for Erlang.
%%%
%%% To read a description of binary format, run
%%% <code>info '(gettext) MO Files'</code>
%%% or visit http://www.gnu.org/software/gettext/manual/gettext.html#MO-Files
%%%
%%% Produce [{{Singular, Plurals, Context}, TransPluralsPhrases::[binary()]}] as
%%% output.
%%% Eg, for .po file (converted to .mo)
%%% <pre>
%%% msgid "Download"
%%% msgctx "some-ctx"
%%% msgstr "Скачать"
%%%
%%% msgid "Stone"
%%% msgid_plural "Stones"
%%% msgstr[0] "Камень"
%%% msgstr[1] "Камня"
%%% msgstr[2] "Камней"
%%% </pre>
%%% it will produce
%%% <pre><code>
%%% [{ {<<"Download">>, undefined, <<"some-ctx">>}, [<<"Скачать">>] },
%%%  { {<<"Stone">>, <<"Stones">>, undefined}, [<<"Камень">>, <<"Камня">>, <<"Камней">>] }]
%%% </code></pre>
%%% @end
%%% Created :  3 Sep 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter_mo_parser).
-export([parse/1, parse_file/1, to_dict/1]).
-export_type([catalog/0, key/0, value/0]).

-type key() :: {Singular :: binary(),
                Plural :: binary() | undefined,
                Context :: binary() | undefined}.
-type value() :: [Phrase :: binary()].

-type catalog() :: [{key(), value()}].

-record(st,
        {bin :: binary(),
         obin :: binary(),
         catalog=[] :: catalog(),
         bo :: little | big,
         version :: integer(),
         msg_cnt :: integer(),
         orig_tab_offset :: integer(),
         trans_tab_offset :: integer()}).

-spec parse_file(file:filename()) -> #st{}.
parse_file(Name) when is_list(Name) ->
    {ok, Bin} = file:read_file(Name),
    parse(Bin).

-spec parse(binary()) -> #st{}.
parse(Bin) when is_binary(Bin) ->
    State = #st{bin=Bin, obin=Bin},
    State2 = parse_magick(State),
    State3 = parse_meta(State2),
    parse_catalog(State3, 0).

-spec to_dict(#st{}) -> catalog().
to_dict(#st{catalog=Catalog}) ->
    Catalog.

parse_magick(#st{bin = <<16#950412de:32/little, Ver:32/little, Rest/binary>>} = S) ->
    S#st{bo=little, version=Ver, bin=Rest};
parse_magick(#st{bin = <<16#950412de:32/big, Ver:32/big, Rest/binary>>} = S) ->
    S#st{bo=big, version=Ver, bin=Rest}.


parse_meta(#st{bo=little, bin = <<MsgCnt:32/little, OrigTabOffset:32/little,
                                  TransTabOffset:32/little, Rest/binary>>} = S) ->
    S#st{msg_cnt = MsgCnt, orig_tab_offset = OrigTabOffset,
         trans_tab_offset = TransTabOffset, bin=Rest};
parse_meta(#st{bo=big, bin = <<MsgCnt:32/big, OrigTabOffset:32/big,
                               TransTabOffset:32/big, Rest/binary>>} = S) ->
    S#st{msg_cnt = MsgCnt, orig_tab_offset = OrigTabOffset,
         trans_tab_offset = TransTabOffset, bin=Rest}.


parse_catalog(#st{msg_cnt=N, catalog=Cat} = S, N) ->
    S#st{catalog=lists:reverse(Cat)};
parse_catalog(#st{orig_tab_offset=OrigO, trans_tab_offset=TransO,
                  obin=Bin, bo=Bo, catalog=Catalog} = S, N) ->
    OrigBin = get_string(N, OrigO, Bin, Bo),
    TransBin = get_string(N, TransO, Bin, Bo),
    NewCatalog = [{parse_orig(OrigBin), parse_trans(TransBin)} | Catalog],
    parse_catalog(S#st{catalog=NewCatalog}, N + 1).

get_string(N, O, Bin, little) ->
    O1 = O + 8 * N,
    <<_:O1/binary, Len:32/little, StringO:32/little, _/binary>> = Bin,
    get_string1(StringO, Len, Bin);
get_string(N, O, Bin, big) ->
    O1 = O + 8 * N,
    <<_:O1/binary, Len:32/big, StringO:32/big, _/binary>> = Bin,
    get_string1(StringO, Len, Bin).

get_string1(StringO, Len, Bin) ->
    <<_:StringO/binary, String:Len/binary, _/binary>> = Bin,
    String.

-spec parse_orig(binary()) -> key().
parse_orig(OrigBin) ->
    %% msgctx<EOT>msgid<\0>msgid_plural
    {Context, Rest} = case binary:split(OrigBin, <<4>>) of
                          [OrigBin] -> {undefined, OrigBin};
                          Parts -> list_to_tuple(Parts)
                      end,
    case binary:split(Rest, <<0>>) of
        [Rest] -> {Rest, undefined, Context};
        [Singular, Plural] -> {Singular, Plural, Context}
    end.

-spec parse_trans(binary()) -> value().
parse_trans(TransBin) ->
    %% split by \0 to plural forms
    binary:split(TransBin, <<0>>, [global]).
