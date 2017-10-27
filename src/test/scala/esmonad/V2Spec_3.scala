package esmonad

import org.scalatest._

/**
  * This is an example of some sugar on creating the handler (not visible here)
  */
class V2Spec_3 extends FlatSpec with Matchers {
  import esmonad.V2_3._

  "The V2_3 object" should "be valid" in {

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
