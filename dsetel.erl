%% destructive setelement example

-module(dsetel).

-export([f/1]).

-record(r, {a=1, b=true, c="xy", d, e, f, g, h}).

f(X) ->
    R0 = r(X),
    R1 = R0#r{b=false, c="", g=42},
    m(R1).

r(X) ->
    #r{a=X}.

m(#r{c="xy", a=0}) -> one;
m(#r{c="xy", a=1}) -> two;
m(#r{c="xz", a=0}) -> three;
m(#r{c="xz", a=1}) -> four;
m(_) -> other.
