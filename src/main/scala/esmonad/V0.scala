package esmonad

trait V0Models {

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {

    def turn(turtle: Turtle, rot: Rotation): Turtle =
      turtle.copy(dir = Direction.rotate(turtle.dir, rot))

    def walk(turtle: Turtle, dist: Int): Turtle =
      turtle.copy(pos = Position.move(turtle.pos, turtle.dir, dist))

  }

}

object V0 extends V0Models
