%% Using standard list functions with inlining
%% +inline
%% +inline_list_funcs

-module(listfuncs).

-export([length/1]).

inc(A) -> A + 1.

length(List) ->
    lists:foldl(fun (_X, A) -> inc(A) end, 0, List).
