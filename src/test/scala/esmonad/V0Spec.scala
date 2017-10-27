package esmonad

import org.scalatest._

/**
 * This is an example of using the model without any event sourcing in the first place.
 */
class V0Spec extends FlatSpec with Matchers {
  import esmonad.V0._

  "The V1 object" should "be valid" in {

    def moveRight(turtle: Turtle, dist: Int) = {
      val moved = Turtle.walk(turtle, dist)
      Turtle.turn(moved, ToRight)
    }

    val state1 = Turtle("123", Position.zero, North)
    val state2 = moveRight(state1, 1)
    val state3 = moveRight(state2, 1)
    val state4 = moveRight(state3, 2)
    val state5 = moveRight(state4, 2)

    state5 shouldBe Turtle("123", Position(-1, -1), North)

  }

}
