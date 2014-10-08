

Relation between a graph and its Strongly Connected Components (SCCs).

Usage:

    nodes_arcs_sccs(+Ns, +As, -SCCs)

where:

* `Ns` is a list of nodes. Each node must be a ground term.
* `As` is a list of `arc(N1,N2)` terms where `N1` and `N2` are nodes.
* `SCCs` is a list of lists of nodes that are in the same strongly
  connected component.

Running time is `O(|V| + log(|V|)*|E|)`.

Example:

```
?- nodes_arcs_sccs([a,b,c,d], [arc(a,b),arc(b,a),arc(b,c)], SCCs).
SCCs = [[a,b],[c],[d]].
```
