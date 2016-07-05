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
        val boxes = randomColorBoxes(n - 1)
        (box |@| boxes) map { (b, bs) => b beside bs }
    }

  randomColorBoxes(5).run.draw

  def makePoint(x: Random[Double], y: Random[Double]): Random[Point] = {
    x |@| y map { (x, y) => Point.cartesian(x, y) }
  }
//
  val normal = Random.normal(50, 15)
  val normal2D = makePoint(normal, normal)
//
  def MakeManyPoints(numberOfPointsToMake: Int): List[Random[Point]] = {
    val data = (1 to numberOfPointsToMake).toList.map(_ => normal2D)
    data
  }

  val data = MakeManyPoints(1000)

  def CreateImageFromPoint(point: Point): Image = {
    circle(2).at(point.toVec)
  }

  def ConvertPointsToImages(points: List[Random[Point]]): List[Random[Image]] = {
    points map (x => x.map(z => CreateImageFromPoint(z)))
  }

  val images = ConvertPointsToImages(data)

  def AllOnImages(points: List[Random[Image]]): Random[Image] =
    points match {
      case Nil => Random.always(Image.empty)
      case img :: imgs => img |@| AllOnImages(imgs) map { (i, is) => i on is }
    }

  val finalPlot = AllOnImages(images)

  finalPlot.run.draw

  def perturb(point: Point): Random[Point] = {
    Random.normal(0, 10) |@| Random.normal(0, 10) map { (x,y) =>
      Point.cartesian(point.x + x, point.y + y)
    }
  }

  def rose(k: Int): Angle => Point =
    (angle: Angle) => {
      Point.cartesian((angle * k).cos * angle.cos, (angle * k).cos * angle.sin)
    }

  def disturbedRose(k:Int) = {
    (angle: Angle) => {
      perturb(Point.cartesian((angle * k).cos * angle.cos, (angle * k).cos * angle.sin))
    }
  }

  def perturbedRose2(k: Int): Angle => Random[Point] =
    rose(k) andThen perturb



}
