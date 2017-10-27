package esmonad

trait V0Models {

  case class Turtle(id: String, pos: Position, dir: Direction)

  def withinRange(pos: Position): Boolean = pos.x.abs < 100 && pos.y.abs < 100

object Turtle {

  def create(id: String, pos: Position, dir: Direction): Either[String, Turtle] = {
    if (withinRange(pos)) Right(Turtle(id, pos, dir))
    else Left("Too far away")
  }

  def turn(rot: Rotation)(turtle: Turtle): Either[String, Turtle] =
    Right(turtle.copy(dir = turtle.dir.rotate(rot)))

  def walk(dist: Int)(turtle: Turtle): Either[String, Turtle] = {
    val moved = turtle.pos.move(turtle.dir, dist)
    if (withinRange(moved)) Right(turtle.copy(pos = turtle.pos.move(turtle.dir, dist)))
    else Left("Too far away")

  }

}

}

object V0 extends V0Models
