package esmonad

import org.scalatest._

class V1Spec extends FlatSpec with Matchers {

  "The V1 object" should "be valid" in {

    import V1App._

    val events = Seq(Look(West), Forward(2), Look(South))
    val initialState = Turtle(0, 0, North)
    val finalState = events.foldLeft(initialState)(handler)
    finalState shouldBe Turtle(-2, 0, South)

  }

}
