package esmonad

import org.scalatest._

/**
 * This is an example of using the model without any event sourcing in the first place.
 */
class V0Spec_2 extends FlatSpec with Matchers {
  import esmonad.V0._

  "The V0 object" should "be valid" in {

    def walkRight(dist: Int)(turtle: Turtle) = {
      Turtle.walk(dist)(turtle)
        .flatMap(Turtle.turn(ToRight))
    }

    val state1 = Turtle("123", Position.zero, North)
    val state2 = walkRight(1)(state1)
      .flatMap(walkRight(1))
      .flatMap(walkRight(2))
      .flatMap(walkRight(2))
    state2 shouldBe Turtle("123", Position(-1, -1), North)

  }

}
