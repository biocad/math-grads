# Math.Grads

[![Travis](https://img.shields.io/travis/biocad/math-grads.svg)](https://travis-ci.org/biocad/math-grads)
[![hackage](https://img.shields.io/hackage/v/math-grads.svg)](https://hackage.haskell.org/package/math-grads)
[![hackage-deps](https://img.shields.io/hackage-deps/v/math-grads.svg)](https://hackage.haskell.org/package/math-grads)

Math.Grads is library that provides graph-like data structures
and various useful algorithms for analysis of these data structures.

Its main feature is that all of provided type classes, data structures and 
functions are written in most abstract way possible to meet different demands
in functionality.

## Data Structures

### Graph

Graph is a type class that upon being instantiated gives data structure
properties of graph-like object.

### GenericGraph

GenericGraph is a data structure that describes undirected graphs and is
parametrized by type of graph's vertices and type of graph's edges. 
So it's really up to the developer what will be stored in Generic Graph's vertices
and edges.

GenericGraph is honest instance of Graph, therefore it can be used in all functions
that require their parameters to be Graphs.

## Algorithms

### Ullman's subgraph isomorphism algorithm

Math.Grads contains implementation of Ullman's subgraph isomorphism 
[algorithm](https://www.cs.bgu.ac.il/~dinitz/Course/SS-12/Ullman_Algorithm.pdf).
There are several functions that one can find helpful in order to check two graphs
for isomorphism or subgraph isomorphism: 

* `isIso` checks whether two graphs are isomorphic;
* `isIsoSub` checks whether second graph has subgraph isomorphic to the first one;
* `getIso` finds matching of vertices of first graph to vertices of subgraph in second graph that
is isomorphic to the first graph;
* `getMultiIso` finds all such matchings.

In order for these functions to work graphs that are being passed to them have to also
be instances of `GComparable` type class.

Definition of this class is as follows:

```haskell
class (Graph g1, Graph g2) => GComparable g1 v1 e1 g2 v2 e2 where
  vComparator :: g1 v1 e1 -> g2 v2 e2 -> VComparator v1 v2
  eComparator :: g1 v1 e1 -> g2 v2 e2 -> EComparator e1 e2

-- | Function that checks whether two vertices are identical.
type VComparator v1 v2 = VertexIndex -> VertexIndex -> Bool

-- | Function that checks whether two edges are identical.
type EComparator e1 e2 = GraphEdge e1 -> GraphEdge e2 -> Bool
```

So, basically, if two `Graph`s are `GComparable` with each other there exist
two functions that are responsible for establishing equality between vertices and edges
of such Graphs.

Here Math.Grads gets its chance to shine, because developer isn't constrained to 
what we (as developers of Math.Grads) thought would be an appropriate way for comparing 
vertices and edges of your data structure. We give the developers opportunity to define 
such relations for their data structures themselves. 

Maybe you want to know surroundings of two vertices in order to compare them, maybe 
you don't — the choice is yours!

### Algorithm for calculation of planar graph's coordinates

Math.Grads provides algorithm for calculation of coordinates of planar graphs.
Its main idea is that most such graphs used in practice can be represented 
as union of systems of conjugated cycles and paths that connect these systems.

So, if you know, that your planar graph looks just like this 
(for example, small molecules from chemistry perfectly fit 
into the definition of graphs that can be drawn correctly by the algorithm), 
you may find `getCoordsForGraph` function quite useful.

Algorithm first draws systems of conjugated cycles, then draws paths between them,
unites systems with path and using random generator samples different conformations
of resulting graph until conformation without self-intersections (that's why graph needs
to be planar) is found.

Once again, in order for you graph to be drawn you need to make it an instance of
special type class:

```haskell
class Graph g => Drawable g v e where
  edgeFixator :: g v e -> EdgeFixator e
  edgeFixator = const $ (,) []
  
type EdgeFixator e = CoordMap -> (EdgeList e, CoordMap)
```

`edgeFixator` is function that given `Graph` returns other function that somehow transforms 
coordinates of graph before sampling and states, which edges of graph shouldn't change their coordinates
during sampling ('fixates' them, if you will). As you can see, `edgeFixator` has default implementation,
so if you don't want such functionality, just instantiate your graph as `Drawable` without
getting into such details.

### Miscellaneous functions on graphs

Math.Grads also provides all other kinds of graph algorithms that you might find useful:
your depth-first searches, breadth-first searches, functions to find cycles in graphs and so on.
