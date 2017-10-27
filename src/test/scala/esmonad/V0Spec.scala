package esmonad

import org.scalatest._

/**
 * This is an example of using the model without any event sourcing in the first place.
 */
class V0Spec extends FlatSpec with Matchers {
  import esmonad.V0._

  "The V0 object" should "be valid" in {

    def walkRight(dist: Int)(state: Turtle) = {
      for {
        state1 <- Turtle.walk(dist)(state)
        state2 <- Turtle.turn(ToRight)(state1)
      } yield state2
    }

    val state = for {
      state1 <- Turtle.create("123", Position.zero, North)
      state2 <- walkRight(1)(state1)
      state3 <- walkRight(1)(state2)
      state4 <- walkRight(2)(state3)
      state5 <- walkRight(2)(state4)
    } yield state5

    state shouldBe Right(Turtle("123", Position(-1, -1), North))

  }

}
