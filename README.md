Overview
--------
This is a library for computing permutation representations of circle puzzles. A circle puzzle consists of a set of intersecting disks in the plane, each of which rotates by a fixed angle. See [here](http://www.jaapsch.net/puzzles/circleman.htm) for an introduction to these types of puzzles. This library was created with the goal of analyzing circle puzzles using a system for computational group theory like [GAP](http://www.gap-system.org/).

Here's a brief overview of the packages:
* `math`: Utilities including fixed-point arithmetic and classes for manipulating arcs and circles.
* `puzzle`: Classes for things that only make sense in the context of a puzzle. This is where we compute cut sets, parts, and permutation representations associated with a puzzle.
* `example`: Example usage.
In general, code is documented wherever possible.

Usage
-----
This is implemented as a [Scala](http://www.scala-lang.org/) library. It also uses the [BigDecimalMath](https://arxiv.org/abs/0908.3030) library, which is not included.


`src/circlepuzzles/example/Example.scala` shows how to define a puzzle in terms of its moves and get its permutation representation. For other queries, see the Scaladoc.

Known Issues
------------
* BigDecimalMath's sine and cosine functions throw exceptions on inputs very close to integer multiples of pi/2. This may later be addressed by removing entirely the dependence on BigDecimalMath and implementing these ourselves.
* The algorithm for computing part boundaries assumes that part boundaries are simply connected (i.e. moving parts do not have holes). In general, moving parts can only have holes if one move circle is fully contained in another move circle, and their cuts never intersect.
