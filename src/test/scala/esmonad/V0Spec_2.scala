package esmonad

import org.scalatest._

/**
 * This is an example of using the model without any event sourcing in the first place.
 */
class V0Spec_2 extends FlatSpec with Matchers {
  import esmonad.V0._

  "The V0 object" should "be valid" in {

    def walkRight(dist: Int) = {
      (Turtle.walk(dist) _)
        .andThen(Turtle.turn(ToRight))
    }

    val state1 = Turtle("123", Position.zero, North)
    val state2 = walkRight(1)
      .andThen(walkRight(1))
      .andThen(walkRight(2))
      .andThen(walkRight(2))
      .apply(state1)
    state2 shouldBe Turtle("123", Position(-1, -1), North)

  }

}
