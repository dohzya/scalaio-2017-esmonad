package esmonad

import org.scalatest._

class V2Spec extends FlatSpec with Matchers {
  import esmonad.V2App._

  "The V2 object" should "be valid" in {

    val events = Seq(
      Create("123", Position.zero, North),
      Walk("123", 1), Turn("123", ToRight),
      Walk("123", 1), Turn("123", ToRight),
      Walk("123", 2), Turn("123", ToRight),
      Walk("123", 2),
    )
    val finalState = events.foldLeft(Option.empty[Turtle]) {
      case (state, event) => Some(handler(state, event))
    }
    finalState shouldBe Some(Turtle("123", Position(-1, -1), West))

  }

}
