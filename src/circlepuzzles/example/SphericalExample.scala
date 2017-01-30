package circlepuzzles.example

import circlepuzzles.geometry.Angle
import circlepuzzles.geometry.spherical._
import circlepuzzles.math.FixedPoint
import circlepuzzles.puzzle.SphericalPuzzle._

/**
  * Example showing how to define a spherical puzzle and get its permutation representation.
  */
object SphericalExample {
  def main(args: Array[String]): Unit = {
    // We'll make a puzzle with two deep cuts: one of order 2 at a cube center, the other of order 3 at a cube corner
    // Deep cut means that the radius in radians is pi / 2
    val r = new Angle(FixedPoint.HalfPi)
    // Move at the cube center is centered at (1, 0, 0)
    val move0 = Move(Disk(Point(FixedPoint.One, FixedPoint.Zero, FixedPoint.Zero), r), 2)
    // Move at the cube corner is centered at (sqrt(3)/3, sqrt(3)/3, sqrt(3)/3)
    val root3Over3 = FixedPoint.sqrt(FixedPoint(3)) / FixedPoint(3)
    val move1 = Move(Disk(Point(root3Over3, root3Over3, root3Over3), r), 3)
    // Make a puzzle with the two moves, and have it automatically cache all computations
    val puzzle = new LazyCachingPuzzle(move0, move1)
    // Print out a permutation representation that GAP can read
    for(((_, permutation), index) <- puzzle.permutationStrings.zipWithIndex) {
      println("move" + index + " := AsPermutation(Transformation(" + permutation + "));")
    }
  }
}
