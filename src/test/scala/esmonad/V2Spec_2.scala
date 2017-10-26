package esmonad

import org.scalatest._

/**
 * This is an example of some sugar on using the handler
 */
class V2Spec_2 extends FlatSpec with Matchers {
  import esmonad.V2_2._

  "The V2_2 object" should "be valid" in {

    val events = Seq(
      Create("123", Position.zero, North),
      Walk("123", 1), Turn("123", ToRight),
      Walk("123", 1), Turn("123", ToRight),
      Walk("123", 2), Turn("123", ToRight),
      Walk("123", 2),
    )
    val finalState = events.foldLeft(Option.empty[Turtle])(Turtle.handler)
    finalState shouldBe Some(Turtle("123", Position(-1, -1), West))

  }

}
