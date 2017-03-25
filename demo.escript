#!/usr/bin/env escript
%%  -*- erlang -*-
%%! +A0

%% To run one of the examples, say e.g. "./demo.escript coding"
%% or "./demo.escript coding raw"

-module(script).
-mode(compile).
-compile([export_all, nowarn_export_all]).

%% example runner
main([Label|Args]) ->
    try ?MODULE:(list_to_atom(Label))(Args) of
        Val ->
            io:format(standard_io, "~tp.\n", [Val])
    catch
        Class:Term ->
            io:format(standard_error, "~tp.\n",
                      [{error, {Class, Term, erlang:get_stacktrace()}}])
    end.



%% Note: c:c(...) is the function in the module c.erl (documented, but more
%% or less an obscure implementation detail) that implements the Erlang
%% shell command shortcuts like c(...) and q(). c(...) is just a frontend to
%% compile:file(...), which also loads the module if compilation succeeds.
%% The erlc shell command calls erl -s erl_compile to run compile:file().

coding(["raw"]) ->
    %% character dump

    io:format("latin:~n~s~n~s~n", [os:cmd("cat latin.erl"),
                                   os:cmd("od -c latin.erl")]),

    io:format("utf_8:~n~s~n~s~n", [os:cmd("cat utf_8.erl"),
                                   os:cmd("od -c utf_8.erl")]);


coding([]) ->
    %% programs do the same thing regardless of encoding

    c:c(latin),
    c:c(utf_8),
    io:format("~nlatin: ~w~nutf_8: ~w~n~n", [latin:f(), utf_8:f()]).



tokens([]) ->
    %% read latin-1 bytes and convert to string

    {ok, LatinBin} = file:read_file("latin.erl"),
    LatinTxt = binary_to_list(LatinBin),

    %% read utf-8 bytes and convert to string

    {ok, Utf8Bin} = file:read_file("utf_8.erl"),
    Utf8Txt = unicode:characters_to_list(Utf8Bin),

    %% tokenize

    {ok, LatinTokens, _} = erl_scan:string(LatinTxt),
    io:format("~nlatin:~n~w~n", [LatinTokens]),

    {ok, Utf8Tokens, _} = erl_scan:string(Utf8Txt),
    io:format("~nutf_8:~n~w~n~n", [Utf8Tokens]).



dot([]) ->
    %% separation of "forms" - period followed by whitespace or not?

    {ok, DotTs, _} = erl_scan:string( "foo. bar." ),
    io:format("~ndot: ~w~n", [DotTs]),

    {ok, Ts, _} = erl_scan:string( "foo.bar" ),
    io:format("~nnot-a-dot: ~w~n~n", [Ts]).



%% program text as a string
prg0() ->
    (
      "%% Small example module\n"
      "-module(mod).\n\n"
      "-export([f/0]).\n\n"
      "%% @doc ...\n"
      "f(X) -> X + 1.\n"
    ).



scan([]) ->
    %% standard scanning, start at line 1

    {ok, Ts, _} = erl_scan:string(prg0(), 1, []),

    io:format("~ndefault: ~p~n~n", [Ts]);


scan(["opt"]) ->
    %% return optional tokens

    {ok, Ts, _}    = erl_scan:string(prg0(), 1, []),

    {ok, TsOpt, _} = erl_scan:string(prg0(), 1, [return_white_spaces,
                                                 return_comments]),

    io:format("~noptional: ~p~n", [TsOpt]),
    io:format("~ndiff: ~p~n", [TsOpt -- Ts]);


scan(["col"]) ->
    %% column numbers, start at {1,1}

    {ok, TsCol, _} = erl_scan:string(prg0(), {1,1}, []),

    io:format("~ncolumns: ~p~n", [TsCol]);


scan(["txt"]) ->
    %% include source text
    {ok, TsTxt, _} = erl_scan:string(prg0(), 1, [text, return]),

    io:format("~ntext: ~p~n", [TsTxt]);


scan(["api"]) ->
    %% erl_scan API for getting properties of tokens (OTP 18)

    {ok, TsTxt, _} = erl_scan:string(prg0(), {1,1}, [text]),
    [_, T | _] = TsTxt,  % pick the 'module' token

    %% use api functions to extract token info, don't match
    io:format("~nlocation: ~p~n", [erl_scan:location(T)]),
    io:format("column: ~p~n",     [erl_scan:column(T)]),
    io:format("line: ~p~n",       [erl_scan:line(T)]),
    io:format("text: ~p~n",       [erl_scan:text(T)]).



epp([]) ->
    %% preprocessor features (see preproc.erl), conditional compilation
    c:c(preproc),
    io:format("~npreproc: ~p~n", [preproc:f()]),

    c:c(preproc, [{d,'TEST'}]),
    io:format("~npreproc TEST: ~p~n~n", [preproc:f()]);


epp(["parse"]) ->
    %% parse via epp and prettyprint the forms

    {ok, Forms} = epp:parse_file("preproc.erl",
                                 [{macros, ['TEST']}]),

    io:format("~nparse:~n~s",
              [[[erl_pp:form(F), "\n"] || F <- Forms]]);


epp(["P"]) ->
    %% easy inspection for debugging purposes

    c:c(preproc, ['P']),    % $ erlc -P preproc.erl

    {ok, Bin} = file:read_file("preproc.P"),
    io:put_chars(Bin);


epp(["forms"]) ->
    %% actual list of forms returned by epp - note eof at the end

    {ok, Forms} = epp:parse_file("preproc.erl",
                                 [{macros, ['TEST']}]),

    io:format("~nforms:~n~p~n", [Forms]).



parse([]) ->
    %% direct scanning and parsing of a single form without epp

    FormTxt = "f(X) -> {foo, X + 1}.",

    {ok, Ts, _} = erl_scan:string(FormTxt),

    {ok, Form} = erl_parse:parse_form(Ts),

    io:format("~nparse:~n~p~n", [Form]);


parse(["forms"]) ->
    %% tokenizing and parsing one form at a time until eof

    PrgTxt = ("f() -> f(0).\n"
              "f(X) -> X + 1.\n"
              "g() -> f(0).\n"),

    {ok, Forms} = parse_forms(PrgTxt, []),

    io:format("~nforms:~n~p~n", [Forms]);


parse(["context"]) ->
    %% parsing with a particular context

    FuncList = "f/1, g/2, h/3",

    FormTxt = "-export([" ++ FuncList ++ "]).",

    {ok, Ts, _} = erl_scan:string(FormTxt),
    {ok, Form} = erl_parse:parse_form(Ts),

    {attribute,_,export,Funcs} = Form,

    io:format("~nfuncs: ~p~n~n", [Funcs]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% using the reentrant token scanner function (see above)             %%

parse_forms(PrgTxt, Forms) ->
    case next_form([], PrgTxt, 1) of
        eof -> {ok, lists:reverse(Forms)};
        {ok, Tokens, Remain} ->
            case erl_parse:parse_form(Tokens) of
                {ok, Form} ->
                    parse_forms(Remain, [Form | Forms]);
                Error -> Error
            end;
        Error -> Error
    end.

next_form(Cont, "", Pos) ->
    next_form(Cont, eof, Pos);
next_form(Cont, Input, Pos) ->
    case erl_scan:tokens(Cont, Input, Pos) of
        {done, Result, Remain} ->
            %% one whole form or end of input
            case Result of
                {ok, Tokens, _EndPos} ->
                    {ok, Tokens, Remain};
                {eof, _EndPos} ->
                    %% end of input, no more tokens
                    eof;
                {error, ErrorInfo, _EndPos} ->
                    %% read error
                    {error, ErrorInfo}
            end;
        {more, _NewCont} ->
            %% incomplete form at end of input
            {error, premature_eof, Cont, Input}
    end.
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



debug_info([]) ->
    %% reading and prettyprinting debug info ("abstract format")

    c:c("preproc.erl", [debug_info]),

    {ok, {_Mod, Chunks}} = beam_lib:chunks("preproc.beam", [abstract_code]),
    [{abstract_code, {raw_abstract_v1, Forms}}] = Chunks,

    io:format("~ndebug info:~n~s",
              [[[erl_pp:form(F), "\n"] || F <- Forms]]).



syntax_tools(["dodger"]) ->
    %% parsing without preprocessor expansion - keep '-define', '-ifdef' etc.
    %% (parse errors will become placeholder "forms" for the error message)

    {ok, Forms} = epp_dodger:parse_file("domain.hrl"),

    %% result is an "extended syntax tree" (erl_syntax) - use erl_prettypr
    io:format("~ndodger:~n~s~n~n",
              [erl_prettypr:format(erl_syntax:form_list(Forms))]);


syntax_tools(["comments"]) ->
    %% extracting comment lines

    Comments = erl_comment_scan:file("preproc.erl"),

    io:format("~ncomments:~n~p~n~n", [Comments]);


syntax_tools(["recomment"]) ->
    %% injecting comment lines in the syntax tree (what edoc does)

    {ok, Forms} = epp_dodger:parse_file("preproc.erl"),
    Comments = erl_comment_scan:file("preproc.erl"),

    NewForms = erl_recomment:recomment_forms(Forms, Comments),

    io:format("~nrecomment:~n~s~n~n",
              [erl_prettypr:format(NewForms)]).



-include_lib("syntax_tools/include/merl.hrl").  % since OTP 18

merl([]) ->
    %% create ASTs the simple way

    X = rand:uniform(100),
    AST = ?Q("case N of _@X@ -> bingo; _ -> nope end"),

    io:format("~nmerl:~n~s~n~n",
              [erl_prettypr:format(AST)]);


merl(["match"]) ->
    %% decompose ASTs the simple way

    X = 77,
    AST = ?Q("{foo, [_@X@, $E, $R, $L]}"),
    S = case AST of
            ?Q("{foo, _@List}") -> erl_syntax:concrete(List);
            _ -> "other"
        end,

    io:format("~nmatch: ~s~n~n", [S]);


merl(["show"]) ->
    %% utility function for visualizing syntax trees

    Text = "f(X, Y) -> {ok, X + Y}.",
    io:nl(),
    AST = merl:quote(Text),

    merl:show(AST).



-compile({parse_transform, cmd_ptrans}).  % see cmd_ptrans.erl

ptrans([]) ->
    %% using a parse transform to substitute stuff at compile time

    Info = {user, {'%cmd%', "whoami"}},

    io:format("~ncmd: ~p~n", [Info]).



core([]) ->
    %% compiling a .erl module and stopping at the core stage

    c:c(preproc, [to_core]),    % $ erlc +to_core preproc.erl

    {ok, Bin} = file:read_file("preproc.core"),
    io:put_chars(Bin);


core(["example"]) ->
    %% compiling a hand-written core program to a .beam module
    %% (make sure to use 'clint' option to sanity check your code)

    c:c(core_example, [from_core, clint]),

    %% $ erlc +from_core +clint core_example.core

    io:format("~nlength []: ~p~n", [core_example:length([])]),
    io:format("~nlength [x]: ~p~n", [core_example:length([x])]),
    io:format("~nlength [x,y,z]: ~p~n", [core_example:length([x,y,z])]),
    io:format("~nlength [1..99]: ~p~n",
              [core_example:length(lists:seq(1,99))]),

    io:format("~nlength 42: ~p~n", [catch core_example:length(42)]);


core(["parse"]) ->
    %% reading, parsing and prettyprinting a core program

    {ok,Bin} = file:read_file("core_example.core"),

    {ok,Toks,_} = core_scan:string(binary_to_list(Bin)),

    {ok,CoreMod} = core_parse:parse(Toks),

    io:format("~ncore parse:~n~s~n~n", [core_pp:format(CoreMod)]);


core(["inline"]) ->
    %% inlining happens on the core level
    %% note use of 'binary' to make compiler return data
    %% instead of creating an output file

    {ok, _Name, CoreMod} = compile:file("core_example",
                                        [from_core, inline,
                                         to_core, binary]),

    io:format("~ncore inline:~n~s~n~n", [core_pp:format(CoreMod)]);


core(["unroll"]) ->
    %% inliner can unroll

    {ok, _Name, CoreMod} = compile:file("core_example",
                                        [from_core, inline,
                                         {inline_unroll, 3},
                                         to_core, binary]),

    io:format("~ncore inline:~n~s~n~n", [core_pp:format(CoreMod)]);


core(["explode"]) ->
    %% inliner can unroll too much - tweak size/effort
    %% (note: for more levels of unrolling, size needs to
    %% be increased as well, or the inline attempt will abort;
    %% interaction is tricky to balance, not user friendly)

    {ok, _Name, CoreMod} = compile:file("core_example",
                                        [from_core, inline,
                                         {inline_size, 100},
                                         {inline_effort, 1000},
                                         {inline_unroll, 3},
                                         to_core, binary]),

    io:format("~ncore explode:~n~s~n~n", [core_pp:format(CoreMod)]);


core(["foldl"]) ->
    %% foldl and other standard functions for working on lists

    c:c(listfuncs, [to_core, inline]),

    {ok, Bin} = file:read_file("listfuncs.core"),
    io:put_chars(Bin);


core(["listfuncs"]) ->
    %% foldl and other standard functions for working on lists

    c:c(listfuncs, [to_core, inline_list_funcs, inline]),

    {ok, Bin} = file:read_file("listfuncs.core"),
    io:put_chars(Bin).



kernel(["core"]) ->
    %% core code for comparison to kernel code below
    %% - explicit record tuple arity and tag check
    %% - record updating as a sequence of setelement
    %% - pattern matching moved from function head to case

    c:c(dsetel, [to_core]),    % $ erlc +to_core dsetel.erl

    {ok, Bin} = file:read_file("dsetel.core"),
    io:put_chars(Bin);


kernel([]) ->
    %% compiling a .erl module and stopping at the kernel stage
    %% - use of destructive setelement
    %% - pattern matching compiled to decision tree

    c:c(dsetel, [to_kernel]),    % $ erlc +to_kernel dsetel.erl

    {ok, Bin} = file:read_file("dsetel.kernel"),
    io:put_chars(Bin);


kernel(["receive"]) ->
    %% receive expression at the kernel stage

    c:c(receiver, [to_kernel]),    % $ erlc +to_kernel receiver.erl

    {ok, Bin} = file:read_file("receiver.kernel"),
    io:put_chars(Bin).



asm([]) ->
    %% compiling to beam assembler

    c:c(core_example, [from_core, to_asm]),

    {ok, Bin} = file:read_file("core_example.S"),
    io:put_chars(Bin);


asm(["receive"]) ->
    %% receive expression in asm

    c:c(receiver, [to_asm]),

    {ok, Bin} = file:read_file("receiver.S"),
    io:put_chars(Bin);


asm(["peek"]) ->
    %% hand-written Beam assembler - here be dragons

    c:c(message, [from_asm]),

    io:format("~ncurrent: ~p~n", [message:current()]),

    self() ! foo,

    io:format("~ncurrent: ~p~n", [message:current()]),
    io:format("~ncurrent: ~p~n", [message:current()]),
    io:format("~ncurrent: ~p~n", [message:current()]),
    receive
        M -> io:format("~nreceived: ~p~n", [M])
    end,
    io:format("~ncurrent: ~p~n", [message:current()]).
