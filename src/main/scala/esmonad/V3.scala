package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// Complete Event Sourced Model
trait V3Models extends FinalEvents with FinalHandlers with FinalJournals {

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {
    def create(id: String, pos: Position, dir: Direction): Either[String, (Turtle, TurtleEvent)] =
      Right(Turtle(id, pos, dir) -> Create(id, pos, dir))

    def turn(rot: Rotation)(turtle: Turtle): Either[String, (Turtle, TurtleEvent)] =
      Right(turtle.copy(dir = turtle.dir.rotate(rot)) -> Turn(turtle.id, rot))

    def walk(dist: Int)(turtle: Turtle): Either[String, (Turtle, TurtleEvent)] = {
      val moved = turtle.pos.move(turtle.dir, dist)
      if (moved.x.abs > 100 || moved.y.abs > 100) Left("Too far away")
      else Right(turtle.copy(pos = turtle.pos.move(turtle.dir, dist)) -> Walk(turtle.id, dist))
    }

    implicit val handler = EventHandler[Turtle, TurtleEvent] {
      case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
      case (Some(t), Turn(id, rot)) if id == t.id =>
        t.copy(dir = t.dir.rotate(rot))
      case (Some(t), Walk(id, dist)) if id == t.id =>
        t.copy(pos = t.pos.move(t.dir, dist))
    }

  }

  implicit object TurtleJournal extends DefaultJournal[Turtle, TurtleEvent](Turtle.handler, _.id)

}

object V3 extends V3Models
