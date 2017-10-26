package esmonad

import org.scalatest._

/**
 * This is an example of introducing basic event sourcing without creation events.
 */
class V1Spec extends FlatSpec with Matchers {
  import esmonad.V1._

  "The V1 object" should "be valid" in {

    val events = Seq(
      Walk(1), Turn(ToRight),
      Walk(1), Turn(ToRight),
      Walk(2), Turn(ToRight),
      Walk(2),
    )
    val initialState = Turtle("123", Position.zero, North)
    val finalState = events.foldLeft(initialState)(Turtle.handler)
    finalState shouldBe Turtle("123", Position(-1, -1), West)

  }

}
