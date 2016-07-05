import doodle.core._
import doodle.turtle.Instruction._
import doodle.turtle._
import doodle.backend.StandardInterpreter._
import doodle.core.Image._
import doodle.core.Point._
import doodle.core._
import doodle.jvm.Java2DCanvas._
import doodle.syntax._

import scala.collection.immutable.Iterable


object Week5 extends App {

//  def polygonWithTurtles(sides: Int, sideLength: Double): Image = {
//    val rotation = Angle.one / sides
//    def iter(n: Int): List[Instruction] =
//      n match {
//        case 0 => Nil
//        case n => turn(rotation) :: forward(sideLength) :: iter(n-1)
//      }
//    Turtle.draw(iter(sides))
//  }
//  polygonWithTurtles(5,10)
//
//  def squareSpiral(steps: Int, distance: Double, angle: Angle, increment: Double): Image = {
//    def iter(n: Int, distance: Double): List[Instruction] = {
//      n match {
//        case 0 => Nil
//        case n => forward(distance) :: turn(angle) :: iter(steps-1, distance + increment)
//      }
//    }
//    Turtle.draw(iter(steps, distance))
//  }

//  def polygonWithTurtle(sides: Int, size: Int): Image = {
//    val rotation = Angle.one / sides
//    val elts: List[Instruction] =
//      (1 to sides).toList.map { i =>
//        turn(rotation) :: forward(size)
//      }
//    Turtle.draw(elts)
//  }



  def polygon(sides: Int, size: Int): Image = {
    val rotation = Angle.one / sides
    val elts =
      (1 to sides).toList.map { i =>
        LineTo(polar(size, rotation * i))
      }
    closedPath(MoveTo(polar(size, Angle.zero)) :: elts)
  }
  val result: Image = polygon(5,50)
  result.draw




  def style(image: Image): Image =
    image.
      lineWidth(6.0).
      lineColor(Color.royalBlue).
      fillColor(Color.skyBlue)


  def DoubleLimits(introList : List[Any]) = {
    introList flatMap(x => List(x,x))
  }

  def Empty(introList : List[Any]) = {
    introList flatMap(x => List())
  }

  println(Empty(List(1,2,3)))
}
//
//final case class Distribution[A] (events: List[(A, Double)]) {
//  def flatMap[B](f: A => Distribution[B]) = {
//    val x = events.flatMap {
//      case (a: A, p: Double) =>
//        f(a).events.map {
//          case (b: B, pb: Double) => (b, pb * p)
//        }
//    }
//  }
////    }.groupBy(_._1).mapValues(_.unzip._2.sum).toList
//}