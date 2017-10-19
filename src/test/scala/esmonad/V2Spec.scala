package esmonad

import org.scalatest._

class V2Spec extends FlatSpec with Matchers {

  "The V2 object" should "be valid" in {

    import V2App._

    val id = "123"
    val events = Seq(Create(id), Look(id, South))
    val state = events.foldLeft(Option.empty[Turtle]) {
      case (state, event) => Some(handler(state, event))
    }
    state shouldBe Some(Turtle("123", 0, 0, South))

    val moreEvents = Seq(Forward(id, 2))
    val newState = moreEvents.foldLeft(state) {
      case (state, event) => Some(handler(state, event))
    }
    newState shouldBe Some(Turtle("123", 0, -2, South))

  }

}
