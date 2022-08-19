
:- [bdd].

:- use_module(library(apply)).
:- use_module(library(gv)).
:- use_module(library(yall)).

view_bdd(BDD) :-
  gv_view({BDD}/[Out]>>export_bdd(BDD, Out),
    [directed(true)]).

save_bdd(File, BDD) :-
  gv_export(File, {BDD}/[Out]>>export_bdd(BDD, Out),
    [directed(true)]).

dot_id(Id):-
  Id is random(9999999999).

dot:dot_attribute_(style).

export_bdd_(BDD, DotNodeId, Leaf0, Leaf1, Out):-
  BDD = node(L, V, R),
  dot_id(DotNodeId),
  dot_node_id(Out, DotNodeId, [label(V)]),
  ((L = leaf(0) -> dot_arc_id(Out, DotNodeId, Leaf0, [style(dashed)]));
    (L = leaf(1) -> dot_arc_id(Out, DotNodeId, Leaf1, [style(dashed)]));
    (export_bdd_(L, LId, Leaf0, Leaf1, Out),
      dot_arc_id(Out, DotNodeId, LId, [style(dashed)]))), !,
  ((R = leaf(0) -> dot_arc_id(Out, DotNodeId, Leaf0));
    (R = leaf(1) -> dot_arc_id(Out, DotNodeId, Leaf1));
    (export_bdd_(R, RId, Leaf0, Leaf1, Out),
      dot_arc_id(Out, DotNodeId, RId))), !.

export_bdd(BDD, Out):-
  dot_id(Leaf0),
  dot_id(Leaf1),
  dot_node_id(Out, Leaf0, [label(0)]),
  dot_node_id(Out, Leaf1, [label(1)]),
  export_bdd_(BDD, _, Leaf0, Leaf1, Out).


  %dot_node(Out, Tree, [color(green),label(Rule)]),