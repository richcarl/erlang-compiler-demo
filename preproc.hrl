%% macros work on token sequences; nasty hacks possible

-define( h_begin(Host),     #hostent{h_name=Host ).
-define( h_aliases(List),   , h_aliases=List     ).
-define( h_addrtype(Type),  , h_addrtype=Type    ).
-define( h_length(Length),  , h_length=Length    ).
-define( h_addr_list(List), , h_addr_list=List   ).
-define( h_end,             }                    ).

%% includes and include_lib

-include_lib("kernel/include/inet.hrl").

-include("domain.hrl").  %% contains conditional definition

%% string token concatenation and stringization of macro arguments

-define(Hostname(N), "node_" ??N "." ?DOMAIN).
