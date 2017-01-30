Overview
--------
This is a library for computing permutation representations of circle puzzles. A classic circle puzzle consists of a set of intersecting disks in the plane, each of which rotates by a fixed angle. See [here](http://www.jaapsch.net/puzzles/circleman.htm) for an introduction to these types of puzzles. More generally, circle puzzles can exist in other plane-like geometries, such as the surface of a sphere. This library was created with the goal of analyzing circle puzzles in various geometries using a system for computational group theory like [GAP](http://www.gap-system.org/).

Here's a brief overview of the packages:
* `math`: Utilities for fixed-point arithmetic.
* `geometry`: Abstractions for plane-like geometries, and related geometric constructions.
  * `planar`: Implementation of planar geometric constructions.
  * `spherical`: Implementation of spherical geometric constructions.
* `puzzle`: Moves, parts, cut sets, permutation representations, and other things that only make sense in the context of a puzzle. Contains abstractions and particular implementations by geometry.
* `example`: Example usage.
In general, code is documented wherever possible.

Usage
-----
This is implemented as a [Scala](http://www.scala-lang.org/) library.


The `example` package shows how to define a puzzle in terms of its moves and get its permutation representation in both planar and spherical cases. For other queries, see the Scaladoc.

Known Issues
------------
* The algorithm for computing part boundaries assumes that part boundaries are simply connected (i.e. moving parts do not have holes). In general, moving parts can only have holes if one move circle is fully contained in another move circle, and their cuts never intersect.
* Numerical instability cannot be ruled out. The precision for fixed-point computations has been set high enough for practical use cases, but this is not guaranteed to always be sufficient. Often, these sorts of issues can be resolved by increasing the value of `ComputeScale` in `FixedPoint.scala`.
