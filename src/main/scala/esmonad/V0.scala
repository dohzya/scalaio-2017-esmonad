package esmonad

trait V0Models {

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {

    def turn(turtle: Turtle, rot: Rotation): Turtle =
      turtle.copy(dir = turtle.dir.rotate(rot))

    def walk(turtle: Turtle, dist: Int): Turtle =
      turtle.copy(pos = turtle.pos.move(turtle.dir, dist))

  }

}

object V0 extends V0Models
