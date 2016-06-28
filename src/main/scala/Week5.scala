import doodle.core._
import doodle.turtle.Instruction._
import doodle.turtle._

object Week5 extends App {

  def polygonWithTurtles(sides: Int, sideLength: Double): Image = {
    val rotation = Angle.one / sides
    def iter(n: Int): List[Instruction] =
      n match {
        case 0 => Nil
        case n => turn(rotation) :: forward(sideLength) :: iter(n-1)
      }
    Turtle.draw(iter(sides))
  }
  polygonWithTurtles(5,10)




  def style(image: Image): Image =
    image.
      lineWidth(6.0).
      lineColor(Color.royalBlue).
      fillColor(Color.skyBlue)
}
