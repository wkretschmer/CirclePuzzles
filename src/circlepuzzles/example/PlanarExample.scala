package circlepuzzles.example

import java.awt.{BorderLayout, Graphics2D}
import java.awt.geom.Arc2D
import javax.swing.{JFrame, JPanel}

import circlepuzzles.math._
import circlepuzzles.geometry.planar._
import circlepuzzles.puzzle.PlanarPuzzle._

/**
  * Example showing how to define a planar puzzle and get its permutation representation. This also visualizes the
  * puzzle's cut set.
  */
object PlanarExample {
  def main(args: Array[String]): Unit = {
    // We'll make a puzzle with two moves, centered at (+-1,0)
    // Both moves have radius 2.5 and turning angle 2*pi/3
    val r = FixedPoint("2.5")
    val n = 3
    val move0 = Move(Circle(Point(-FixedPoint.One, FixedPoint.Zero), r), n)
    val move1 = Move(Circle(Point(FixedPoint.One, FixedPoint.Zero), r), n)
    // Make a puzzle with the two moves, and have it automatically cache all computations
    val puzzle = new LazyCachingPuzzle(move0, move1)
    // Print out a permutation representation that GAP can read
    for(((_, permutation), index) <- puzzle.permutationStrings.zipWithIndex) {
      println("move" + index + " := AsPermutation(Transformation(" + permutation + "));")
    }

    // Let's visualize the puzzle
    val frame = new JFrame("Puzzle")
    val panel = new JPanel()

    // We'll scale it so that one unit is 100 pixels, with a buffer of 50 pixels on either side of the puzzle
    // One could do this scaling automatically without too much difficulty, but for now this is hard-coded
    frame.setSize(800, 600)
    frame.getContentPane.add(panel, BorderLayout.CENTER)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)
    val graphics = panel.getGraphics.create().asInstanceOf[Graphics2D]
    // Iterate through the arcs that make cuts
    for(Arc(Circle(Point(x, y), r), start, end) <- puzzle.flatCuts) {
      // Scale everything to fit in the frame
      val diameter = r.toDouble * 200.0
      // Arc2D takes the coordinates of the corner of a rectangle that bounds the arc's circle
      // So, we have to subtract half the radius from the circle's center
      val xCoord = 400.0 + x.toDouble * 100.0 - diameter / 2
      // Why does Java have inconsistent trigonometric sign conventions?
      // Regardless, we need to invert the y coordinate for this to display correctly
      val yCoord = 300.0 - y.toDouble * 100.0 - diameter / 2
      val startDegrees = math.toDegrees(start.radians.toDouble)
      val endDegrees = math.toDegrees(FixedPoint.mod2Pi(end.radians - start.radians).toDouble)
      val doubleArc = new Arc2D.Double(xCoord, yCoord, diameter, diameter, startDegrees, endDegrees, Arc2D.OPEN)
      graphics.draw(doubleArc)
    }
  }
}
