/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Strongly connected components of a graph.
   Written by Markus Triska (triska@metalevel.at), 2011, 2015, 2016, 2020
   Public domain code. Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(scc, [nodes_arcs_sccs/3]).

:- use_module(library(atts)).
:- use_module(library(clpz)).
:- use_module(library(assoc)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(dcgs)).

:- attribute
        lowlink/1,
        node/1,
        successors/1,
        index/1,
        in_stack/1.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Usage:

   nodes_arcs_sccs(+Ns, +As, -SCCs)

   where:

   Ns is a list of nodes. Each node must be a ground term.
   As is a list of arc(From,To) terms where From and To are nodes.
   SCCs is a list of lists of nodes that are in the same strongly
        connected component.

   Running time is O(|V| + log(|V|)*|E|).

   Example:

   %?- nodes_arcs_sccs([a,b,c,d], [arc(a,b),arc(b,a),arc(b,c)], SCCs).
   %@ SCCs = [[a,b],[c],[d]].

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

nodes_arcs_sccs(Ns, As, Ss) :-
        must_be(list, Ns),
        must_be(list, As),
        catch((maplist(node_var_pair, Ns, Vs, Ps),
               list_to_assoc(Ps, Assoc),
               maplist(attach_arc(Assoc), As),
               scc(Vs, successors),
               maplist(v_with_lowlink, Vs, Ls0),
               keysort(Ls0, Ls1),
               group_pairs_by_key(Ls1, Ss0),
               pairs_values(Ss0, Ss),
               % reset all attributes
               throw(scc(Ss))),
              scc(Ss),
              true).

% Associate a fresh variable with each node, so that attributes can be
% attached to variables that correspond to nodes.

node_var_pair(N, V, N-V) :- put_atts(V, node(N)).

v_with_lowlink(V, L-N) :-
        get_atts(V, lowlink(L)),
        get_atts(V, node(N)).

successors(V, Vs) :-
        (   get_atts(V, successors(Vs)) -> true
        ;   Vs = []
        ).

attach_arc(Assoc, arc(X,Y)) :-
        get_assoc(X, Assoc, VX),
        get_assoc(Y, Assoc, VY),
        successors(VX, Vs),
        put_atts(VX, successors([VY|Vs])).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tarjan's strongly connected components algorithm.

   DCGs are used to implicitly pass around the global index, stack
   and the predicate relating a vertex to its successors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

scc(Vs, Succ) :- phrase(scc(Vs), [s(0,[],Succ)], _).

scc([])     --> [].
scc([V|Vs]) -->
        (   vindex_defined(V) -> scc(Vs)
        ;   scc_(V), scc(Vs)
        ).

scc_(V) -->
        vindex_is_index(V),
        vlowlink_is_index(V),
        index_plus_one,
        s_push(V),
        successors(V, Tos),
        each_edge(Tos, V),
        (   { get_atts(V, index(VI)),
              get_atts(V, lowlink(VI)) } -> pop_stack_to(V, VI)
        ;   []
        ).

vindex_defined(V) --> { get_atts(V, index(_)) }.

vindex_is_index(V) -->
        state(s(Index,_,_)),
        { put_atts(V, index(Index)) }.

vlowlink_is_index(V) -->
        state(s(Index,_,_)),
        { put_atts(V, lowlink(Index)) }.

index_plus_one -->
        state(s(I,Stack,Succ), s(I1,Stack,Succ)),
        { I1 #= I+1 }.

s_push(V)  -->
        state(s(I,Stack,Succ), s(I,[V|Stack],Succ)),
        { put_atts(V, in_stack(true)) }.

vlowlink_min_lowlink(V, VP) -->
        { get_atts(V, lowlink(VL)),
          get_atts(VP, lowlink(VPL)),
          VL1 #= min(VL, VPL),
          put_atts(V, lowlink(VL1)) }.

successors(V, Tos) --> state(s(_,_,Succ)), { call(Succ, V, Tos) }.

pop_stack_to(V, N) -->
        state(s(I,[First|Stack],Succ), s(I,Stack,Succ)),
        { put_atts(First, -in_stack(_)) },
        (   { First == V } -> []
        ;   { put_atts(First, lowlink(N)) },
            pop_stack_to(V, N)
        ).

each_edge([], _) --> [].
each_edge([VP|VPs], V) -->
        (   vindex_defined(VP) ->
            (   v_in_stack(VP) ->
                vlowlink_min_lowlink(V, VP)
            ;   []
            )
        ;   scc_(VP),
            vlowlink_min_lowlink(V, VP)
        ),
        each_edge(VPs, V).

v_in_stack(V) --> { get_atts(V, in_stack(true)) }.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   DCG rules to access the state, using semicontext notation.

   How it works is explained in the DCG primer:

                 https://www.metalevel.at/prolog/dcg
                 ===================================
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

state(S), [S] --> [S].

state(S0, S), [S] --> [S0].
