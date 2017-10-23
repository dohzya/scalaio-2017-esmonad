package esmonad

sealed trait Rotation
case object ToLeft extends Rotation; case object ToRight extends Rotation

sealed trait Direction
case object North extends Direction; case object South extends Direction
case object Est extends Direction; case object West extends Direction
object Direction {
  def rotate(dir: Direction, rot: Rotation): Direction =
    (dir, rot) match {
      case (North, ToLeft) => West; case (North, ToRight) => Est
      case (South, ToLeft) => Est; case (South, ToRight) => West
      case (Est, ToLeft) => North; case (Est, ToRight) => South
      case (West, ToLeft) => South; case (West, ToRight) => North
    }
}

case class Position(x: Int, y: Int)
object Position {
  val zero = Position(0, 0)
  def move(pos: Position, dir: Direction, distance: Int): Position =
    dir match {
      case North => pos.copy(y = pos.y + distance); case South => pos.copy(y = pos.y - distance)
      case Est => pos.copy(x = pos.x + distance); case West => pos.copy(x = pos.x - distance)
    }
}
