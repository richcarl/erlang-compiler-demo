%% preprocessor features
-module(preproc).

-include("preproc.hrl").  % see header file for details

-export([f/0]).

f() ->
     %% no commas between macro calls - confuses tools like edoc
     ?h_begin(?Hostname(17))
     ?h_addrtype(inet)
     ?h_end.
