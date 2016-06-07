import doodle.backend.StandardInterpreter._
import doodle.core.Image._
import doodle.core.Point._
import doodle.core._
import doodle.jvm.Java2DCanvas._
import doodle.syntax._

import scala.collection.immutable.::

object Week4 extends App {


  def b41(): Unit = {
    val triangle =
      List(
        MoveTo(cartesian(0,0)),
        LineTo(cartesian(0, 100)),
        LineTo(cartesian(100, 50)),
        LineTo(cartesian(0, 0))
      )

    val square = List(
      MoveTo(cartesian(0,0)),
      LineTo(cartesian(0, 100)),
      LineTo(cartesian(100, 100)),
      LineTo(cartesian(100, 0)),
      LineTo(cartesian(0, 0))
    )

    val pentagon: List[PathElement] =
      List(
        MoveTo(polar(50, 72.degrees)),
        LineTo(polar(50, 144.degrees)),
        LineTo(polar(50, 216.degrees)),
        LineTo(polar(50, 288.degrees)),
        LineTo(polar(50, 360.degrees))
      )

    val closedPaths =
      style(closedPath(triangle) beside closedPath(square) beside closedPath(pentagon))

    closedPaths.draw
  }

  def b42(): Unit = {
    // Not super clear what this is asking for
  }

  //TODO - deal with bug in the increment method

  def b43() =  {
    def ones(n : Int) : List[Any] = {
      n match
      {
        case 0 => Nil
        case x => 1 :: ones(n - 1)
      }
    }
  }

  def b44() =  {
    def descending(n : Int) : List[Any] = {
      n match
      {
        case 0 => Nil
        case x => n :: descending(n - 1)
      }
    }
  }

  def b45() =  {

    def descending(n : Int) : List[Any] = {
      n match
      {
        case 0 => Nil
        case x => n :: descending(n - 1)
      }
    }

    // smart ass approach
    def ascending(n : Int) : List[Any] = {
      descending(n).reverse
    }

    // better approach
    def ascending2(n : Int) : List[Any] = {
      def inner(TopVal : Int, counter : Int) : List[Any] = {
        TopVal match {
          case 0 => Nil
          case m => counter :: inner(TopVal - 1, counter + 1)
        }
      }
      inner(n,1)
    }

    def fill[A](n : Int, someElem: A) : List[A]= {
      n match {
        case 0 => Nil
        case m => someElem :: fill[A](n-1,someElem)
      }
    }

    def double(intList : List[Int]) : List[Int] = {
      intList match {
        case Nil => Nil
        case head :: tails => (head*2) :: double(tails)
      }
    }

    def product(intList : List[Int]) : Int = {
      intList match {
        case Nil => 1
        case head :: tails => head * product(tails)
      }
    }

    def contains[A](genericList : List[A], element : A) : Boolean = {
      genericList match {
        case Nil => false
        case head::tails => if (head == element) true else contains(tails, element)
      }

    }

    def first[A](genericList : List[A], element : A) : A =  {
      genericList match {
        case Nil => element
        case head::tails => head
      }
    }

    //TODO - looks like we did this differently - worth considering what's better
    def reverse[A](genericList : List[A]) : List[A] = {

      genericList match {
        case Nil => Nil
        case head::tails => reverse(tails) :+ (head)

      }
    }

    def polygon(n : Int, startingRotation : Double, sideLength : Int) : Image = {
      def internalAccumulator(n : Int, startingRotation : Double) : List[PathElement] = {
        n match {
          case 0 => Nil
          case m => LineTo(polar(sideLength,startingRotation.degrees)) :: internalAccumulator(n-1,startingRotation+72)
        }
      }

      val paths = MoveTo(polar(sideLength, startingRotation.degrees)) :: internalAccumulator(n, startingRotation)
      closedPath(paths)

    }




  }

















  def style(image: Image): Image =
    image.
      lineWidth(6.0).
      lineColor(Color.royalBlue).
      fillColor(Color.skyBlue)
}
