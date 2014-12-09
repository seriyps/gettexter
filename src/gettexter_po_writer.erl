%% -------------------------------------------------------------------------
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @copyright 2003 Torbjörn Törnkvist
%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%% @author Emil Falk <emil.falk@textalk.se>
%% @doc Write PO-files

%% TODO:
%% Dynamic header. Make package name, version etc configurable.
%% Update timestamps etc.
%% Tests...

-module(gettexter_po_writer).
-export([write/2, write/3]).

-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).

%% @doc Convert and write POT-files.
%% For more information see write/3.
write(Tab, OutDir) ->
    write(Tab, OutDir, []).

%% @doc Convert and write the POT-files.
%%
%% This function will write a pot-file for each domain in the table.
%%
%% The extracted entries should have the form:
%% {Key, MetaData}
%% Key      :: {Domain, Context, Singular, Plural}
%% MetaData :: [{Filename, LineNr, Comments}]
%%
%% see the gettexter_extract module for more information.
write(Tab, OutDir, Options) ->
    %% Assert that the output directiory exists
    ok = filelib:ensure_dir(OutDir),

    %% Write all domains
    write_pot_files(Tab, OutDir, Options).

%%% ---------------------------------------------------------------------------
%%% Serializing a .pot file
%%% ---------------------------------------------------------------------------

%% @doc Write a pot file for each domain in the dets-table.
%% After a POT-file has been written, the corresponding entries are removed from
%% the ets-table. This is repeated until all domains have been written and the
%% table is empty.
write_pot_files(Tab, OutDir, Options) ->
    case ets:first(Tab) of
        %% There are no more entries in the table so we are done
        '$end_of_table' ->
            ok;
        %% There are more domains that needs to be written
        {Domain, _, _, _} ->
            write_pot_file(Domain, OutDir, Tab, Options),
            write_pot_files(Tab, OutDir, Options)
    end.

%% @doc Write a single domain to a POT-file.
%% This will given a domain, extract all entries related to that domain from the
%% temporary table and then write them to a POT-file. After that all those entries
%% will be removed.
write_pot_file(Domain, OutDir, Tab, Options) ->
    %% Open the POT-file
    {ok, Fd} = open_pot_file(OutDir, Domain),

    %% Get all entries
    Pattern = {{Domain, '_', '_', '_'}, '_'},
    Entries = ets:match_object(Tab, Pattern),

    %% Write it
    write_header(Fd, Domain, Options),
    write_entries(Fd, Entries),
    file:close(Fd),

    %% Remove them from the table
    ets:match_delete(Tab, Pattern).

%% @doc Open a POT-file and assert that it didn't existed before
open_pot_file(Dir, Domain) ->
    PotFile = filename:join([Dir, atom_to_list(Domain) ++ ".pot"]),
    filelib:ensure_dir(PotFile),
    file:open(PotFile, [write]).

%% @doc Write a POT-file header.
%% TODO: Make more dynamic?
write_header(Fd, _Domain, _Options) ->
    io:format(Fd, mk_header(), []).

%% @doc Write all entries to a POT-file.
write_entries(Fd, Entries) ->
    file:write(Fd, lists:map(fun serialize/1, Entries)).

%% @doc Serialize a gettext entry
serialize({Entry, MetaData}) ->
    [serialize_metadata(MetaData),
     serialize_entry(Entry),
     "\n"].

%% @doc Serialize the collected meta-data.
%% There are one meta-data entry for each instance of this particular
%% translatable instance. The collected entries are on the form:
%%     {Filename, LineNr, Comment}
%%
%% This functions merges all meta-data into two types of comment lines:
%%     #. Comments from the source
%%     #: extract_me.erl:50, ...
serialize_metadata(MetaData) ->
    {RevComments, RevReferences} = lists:foldl(fun serialize_metadata_helper/2,
                                               {[], []}, MetaData),
    %% Make sure they appear in the correct order
    Comments = lists:reverse(RevComments),
    References0 = lists:reverse(RevReferences),

    %% Format the reference comment
    References1 = format_references(References0),

    %% The serialized entry
    [Comments,
     References1].

%% @doc Helper function to fold over the meta-data
serialize_metadata_helper({Filename, LineNr, CommentRows}, {CommentAcc, RefAcc}) ->
    %% Format the reference, it should be a pure string.
    Reference = Filename ++ [$: | integer_to_list(LineNr)],

    %% Format the comment, if any
    %% They are represented as lists of rows
    %% Append #. to each row and prepend a newline
    NewCommentAcc = case CommentRows of
                        undefined ->
                            CommentAcc;
                        _ ->
                            Comment = [["#. ", Row, "\n"] || Row <- CommentRows],
                            [Comment | CommentAcc]
                    end,
    {NewCommentAcc, [Reference | RefAcc]}.

%% @doc Accumulate references until we need to wrap the line
format_references(References) ->
    format_references(References, 0, [[]]).

%% No references left, return the lines (reverse them first).
format_references([], _SoFar, [Last | Previous]) ->
    %% Reverse all lines but first format the last line.
    lists:reverse([format_reference_list(Last) | Previous]);
format_references([Reference | Rest], SoFar, [Current | Previous]) ->
    %% Count where the line would end. Add twp for whitespace plus comma.
    Length = length(Reference),
    NewEnd = SoFar + Length + 2,
    case NewEnd =< ?ENDCOL of
        %% The references fit on this line
        true ->
            NewCurrent = [Reference | Current],
            format_references(Rest, NewEnd, [NewCurrent | Previous]);
        %% The line is to long, wrap it and format the filled line
        false ->
            NewPrevious = [format_reference_list(Current) | Previous],
            format_references(Rest, Length, [[Reference] | NewPrevious])
    end.

%% @doc Reverse, and format the reference line
format_reference_list(Line) ->
    ["#: ", string:join(lists:reverse(Line), " "), "\n"].

%% @doc Serialize the essential parts of the gettext entry

%% Singular without context
serialize_entry({_, undefined, Text, undefined}) ->
    [msgid(Text),
     msgstr()];

%% Singular with context
serialize_entry({_Domain, Context, Text, undefined}) ->
    [msgctxt(Context),
     serialize_entry({_Domain, undefined, Text, undefined})];

%% Plural without context
serialize_entry({_, undefined, Singular, Plural}) ->
    [msgid(Singular),
     msgid_plural(Plural),
     msgstr_plural([0,1])];

%% Plural with context
serialize_entry({_Domain, Context, Singular, Plural}) ->
    [msgctxt(Context),
     serialize_entry({_Domain, undefined, Singular, Plural})].

%% @doc Check if a string spans multiple lines
is_multiline(Str) ->
    lists:member($\n, Str).

msgid(Str) ->
    pretty_entry("msgid", Str).

msgid_plural(Str) ->
    pretty_entry("msgid_plural", Str).

msgstr() ->
    "msgstr \"\"\n".

msgstr_i(I) ->
    "msgstr[" ++ integer_to_list(I) ++ "] \"\"\n".

msgstr_plural(Ixs) ->
    lists:concat(lists:map(fun msgstr_i/1, Ixs)).

msgctxt(Str) ->
    pretty_entry("msgctxt", Str).

%% @doc Pretty print a multi-line string
pretty_entry(Entry, Str) ->
    case is_multiline(Str) orelse length(Str) > ?ENDCOL of
        true  -> Entry ++ " \"\"\n" ++ pretty(Str);
        false -> Entry ++ " "       ++ pretty(Str)
    end.

pretty(Str) ->
    pretty(Str, []).

pretty([], Acc) ->
    lists:concat(lists:reverse(Acc));
pretty(Str, Acc) when length(Str) =< ?ENDCOL ->
    pretty([], [pretty_string(Str)|Acc]);
pretty(Str, Acc) ->
    {Line, Rest} = get_line(Str),
    pretty(Rest, [pretty_string(Line)|Acc]).

pretty_string(Str) ->
    "\"" ++ escape_chars(Str) ++ "\"\n".

escape_chars(Str) ->
    F = fun($", Acc)  -> [$\\,$"|Acc];
           ($\\, Acc) -> [$\\,$\\|Acc];
           ($\n, Acc) -> [$\\,$n|Acc];
           (C, Acc)   -> [C|Acc]
        end,
    lists:foldr(F, [], Str).

%%% Split the string into substrings,
%%% aligned around a specific column.
get_line(Str) ->
    get_line(Str, ?SEP, 1, ?ENDCOL, []).

%%% End of string reached.
get_line([], _Sep, _N, _End, Acc) ->
    {lists:reverse(Acc), []};
%%% Eat characters.
get_line([H|T], Sep, N, End, Acc) when N < End ->
    get_line(T, Sep, N+1, End, [H|Acc]);
%%% Ended with a Separator on the End boundary.
get_line([Sep|T], Sep, End, End, Acc) ->
    {lists:reverse([Sep|Acc]), T};
%%% At the end, try to find end of token within
%%% the given constraint, else backup one token.
get_line([H|T] = In, Sep, End, End, Acc) ->
    case find_end(T, Sep) of
        {true, Racc, Rest} ->
            {lists:reverse(Racc ++ [H|Acc]), Rest};
        false ->
            case reverse_tape(Acc, In) of
                {true, Bacc, Rest} ->
                    {lists:reverse(Bacc), Rest};
                {false, Str} ->
                    %%% Ugh...the word is longer than ENDCOL...
                    split_string(Str, ?ENDCOL)
            end
    end.

find_end(Str, Sep) ->
    find_end(Str, Sep, 1, ?PIVOT, []).

find_end([Sep|T], Sep, N, Pivot, Acc) when N =< Pivot ->
    {true, [Sep|Acc], T};
find_end(_Str, _Sep, N, Pivot, _Acc) when N > Pivot ->
    false;
find_end([H|T], Sep, N, Pivot, Acc) ->
    find_end(T, Sep, N+1, Pivot, [H|Acc]);
find_end([], _Sep, _N, _Pivot, Acc) ->
    {true, Acc, []}.

reverse_tape(Acc, Str) ->
    reverse_tape(Acc, Str, ?SEP).

reverse_tape([Sep|_T] = In, Str, Sep) ->
    {true, In, Str};
reverse_tape([H|T], Str, Sep) ->
    reverse_tape(T, [H|Str], Sep);
reverse_tape([], Str, _Sep) ->
    {false, Str}.

split_string(Str, End) ->
    split_string(Str, End, 1, []).

split_string(Str, End, End, Acc) ->
    {lists:reverse(Acc), Str};
split_string([H|T], End, N, Acc) when N < End ->
    split_string(T, End, N+1, [H|Acc]);
split_string([], _End, _N, Acc) ->
    {lists:reverse(Acc), []}.

%% @doc The pot header file
mk_header() ->
    "# SOME DESCRIPTIVE TITLE.\n"
    "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n"
    "# This file is distributed under the same license as the "
    "PACKAGE package.\n"
    "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n"
    "#\n"
    "#, fuzzy\n"
    "msgid \"\"\n"
    "msgstr \"\"\n"
    "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
    "\"POT-Creation-Date: 2003-10-21 16:45+0200\\n\"\n"
    "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
    "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
    "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
    "\"Language: en\\n\""
    "\"MIME-Version: 1.0\\n\"\n"
    "\"Content-Type: text/plain; charset=UTF-8\\n\"\n"
    "\"Content-Transfer-Encoding: 8bit\\n\"\n"
    "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\"\n\n".
