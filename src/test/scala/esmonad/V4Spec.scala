package esmonad

import org.scalatest._

class V4Spec extends FlatSpec with Matchers {

  "The V4 object" should "be valid" in {

    import V4App._

    val id = "123"
    val events = Seq(Create(id), Look(id, South))
    val state = events.foldLeft(Option.empty[Turtle])(handler)
    state shouldBe Some(Turtle("123", 0, 0, South))

    val moreEvents = Seq(Forward(id, 2))
    val newState = moreEvents.foldLeft(state)(handler)
    newState shouldBe Some(Turtle("123", 0, -2, South))

  }

}
