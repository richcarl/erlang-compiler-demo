%% @doc Parse transformation example - compile time os cmd

-module(cmd_ptrans).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

parse_transform(Forms, _Options) ->
    Tree = erl_syntax:form_list(Forms),

    NewTree = postorder(fun map/1, Tree),

    [erl_syntax:revert(Form)
     || Form <- erl_syntax:form_list_elements(NewTree)].

postorder(F, Tree) ->
    F(case erl_syntax:subtrees(Tree) of
          [] ->
              Tree;
          Subtrees ->
              NewSubtrees = [[postorder(F, Subtree)
                              || Subtree <- Group]
                             || Group <- Subtrees],
              erl_syntax:update_tree(Tree, NewSubtrees)
      end).

map(Node) ->
    case Node of
        ?Q("{'%cmd%', _@Txt}") ->
            Cmd = erl_syntax:concrete(Txt),
            erl_syntax:string(os:cmd(Cmd));
        _ ->
            Node
    end.
