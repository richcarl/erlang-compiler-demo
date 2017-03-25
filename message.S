%% (c) 2005 Tony Rogvall
{module, message}.

{exports, [{first,0},
	   {next,0}, {next,1},
	   {remove,0},
	   {current,0},
	   {module_info,0},
	   {module_info,1}]}.

{attributes, []}.

{labels, 24}.

%% current() -> empty | {message,M}
%%
%% Description:
%%    Peek the current message

{function, current, 0, 2}.
{label,1}.
    {func_info,{atom,message},{atom,current},0}.
{label,2}.
    {loop_rec,{f,3},{x,0}}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,message}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    return.
{label,3}.
    {move,{atom,empty},{x,0}}.
    return.
