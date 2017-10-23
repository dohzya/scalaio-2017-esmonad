package esmonad

object V0App extends V0 with App

trait V0 {

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {

    def turn(turtle: Turtle, rot: Rotation): Turtle =
      turtle.copy(dir = Direction.rotate(turtle.dir, rot))

    def walk(turtle: Turtle, dist: Int): Turtle =
      turtle.copy(pos = Position.move(turtle.pos, turtle.dir, dist))

  }

}
