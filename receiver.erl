%% receive example

-module(receiver).

-export([r/1]).

r(From) ->
    receive
        {From, Data} -> Data
    after 5000 ->
            timeout
    end.
