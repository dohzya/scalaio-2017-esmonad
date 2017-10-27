package esmonad

import org.scalatest._

/**
 * This is an example of using the model without any event sourcing in the first place.
 */
class V0Spec extends FlatSpec with Matchers {
  import esmonad.V0._

  "The V0 object" should "be valid" in {

    def moveRight(dist: Int)(turtle: Turtle) = {
      val moved = Turtle.walk(dist)(turtle)
      Turtle.turn(ToRight)(moved)
    }

    val state1 = Turtle("123", Position.zero, North)
    val state2 = moveRight(1)(state1)
    val state3 = moveRight(1)(state2)
    val state4 = moveRight(2)(state3)
    val state5 = moveRight(2)(state4)

    state5 shouldBe Turtle("123", Position(-1, -1), North)

  }

}
