package esmonad

import org.scalatest._

class V3Spec extends FlatSpec with Matchers {
  import esmonad.V3App._

  "The V3 object" should "be valid" in {

    val events = Seq(
      Create("123", Position.zero, North),
      Walk("123", 1), Turn("123", ToRight),
      Walk("123", 1), Turn("123", ToRight),
      Walk("123", 2), Turn("123", ToRight),
      Walk("123", 2),
    )
    val finalState = events.foldLeft(Option.empty[Turtle])(handler)
    finalState shouldBe Some(Turtle("123", Position(-1, -1), West))

  }

}
