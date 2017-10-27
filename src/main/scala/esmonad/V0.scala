package esmonad

trait V0Models {

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {

    def create(id: String, pos: Position, dir: Direction): Either[String, Turtle] = {
      if (pos.x.abs > 100 || pos.y.abs > 100) Left("Too far away")
      else Right(Turtle(id, pos, dir))
    }

    def turn(rot: Rotation)(turtle: Turtle): Either[String, Turtle] =
      Right(turtle.copy(dir = turtle.dir.rotate(rot)))

    def walk(dist: Int)(turtle: Turtle): Either[String, Turtle] = {
      val moved = turtle.pos.move(turtle.dir, dist)
      if (moved.x.abs > 100 || moved.y.abs > 100) Left("Too far away")
      else Right(turtle.copy(pos = turtle.pos.move(turtle.dir, dist)))
    }

  }

}

object V0 extends V0Models
