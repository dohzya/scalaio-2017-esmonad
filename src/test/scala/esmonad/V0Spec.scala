package esmonad

import org.scalatest._

class V0Spec extends FlatSpec with Matchers {
  import esmonad.V0._

  "The V1 object" should "be valid" in {

    val state1 = Turtle("123", Position.zero, North)
    val state2 = Turtle.walk(state1, 1)
    val state3 = Turtle.turn(state2, ToRight)
    val state4 = Turtle.walk(state3, 1)
    val state5 = Turtle.turn(state4, ToRight)
    val state6 = Turtle.walk(state5, 2)
    val state7 = Turtle.turn(state6, ToRight)
    val state8 = Turtle.walk(state7, 2)

    state8 shouldBe Turtle("123", Position(-1, -1), West)

  }

}
