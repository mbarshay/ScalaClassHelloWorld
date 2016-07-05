import doodle.core._
import doodle.turtle.Instruction._
import doodle.turtle._
import doodle.backend.StandardInterpreter._
import doodle.core.Image._
import doodle.core.Point._
import doodle.core._
import doodle.jvm.Java2DCanvas._
import doodle.syntax._
import doodle.random._
import cats.syntax.cartesian._


object Week6 extends App {


  val randomAngle: Random[Angle] =
    Random.double.map(x => x.turns)

  val randomColor: Random[Color] =
    randomAngle map (hue => Color.hsl(hue, 0.7.normalized, 0.7.normalized))

  def coloredRectangle(color: Color): Image =
    rectangle(20, 20) fillColor color

  def randomColorBoxes(n: Int): Random[Image] =
    n match {
      case 0 => randomColor map { c => coloredRectangle(c) }
      case n =>
        val box = randomColor map { c => coloredRectangle(c) }
        val boxes = randomColorBoxes(n-1)
        (box |@| boxes) map { (b, bs) => b beside bs }
    }

  randomColorBoxes(5).map(x => x.draw)

  def makePoint(x: Random[Double], y: Random[Double]): Random[Point] = {
    x |@| y map { (x, y) => Point.cartesian(x, y) }
  }

  val normal = Random.normal(50, 15)
  val normal2D = makePoint(normal, normal)

  def MakeManyPoints(numberOfPointsToMake : Int) : List[Random[Point]] = {
    val data = (1 to numberOfPointsToMake).toList.map(_ => normal2D)
    data
  }

  def CreateImageFromPoint(point: Point) : Image = {
    //    circle(2).fillColor(Color.cadetBlue.alpha(0.3.normalized)).noLine.at(point.toVec)
    circle(2).at(point.toVec)
  }

  def ConvertPointsToImages(points : List[Random[Point]]) : List[Random[Image]] = {
    points map( x=> x.map(z => CreateImageFromPoint(z)))
  }
//
//  Now create a method that transforms a List[Random[Image]] to a Random[Image] by placing all the points
//    on each other. This is the equivalent of the allOn method we’ve developed previously, but it now works with
//  data wrapped in Random.

  





  // randomColorBoxes: (n: Int)doodle.random.Random[doodle.core.Image]

}
