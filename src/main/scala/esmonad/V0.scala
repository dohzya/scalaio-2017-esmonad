package esmonad

trait V0Models {

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {

    def turn(rot: Rotation)(turtle: Turtle): Turtle =
      turtle.copy(dir = turtle.dir.rotate(rot))

    def walk(dist: Int)(turtle: Turtle): Turtle =
      turtle.copy(pos = turtle.pos.move(turtle.dir, dist))

  }

}

object V0 extends V0Models
