package esmonad

import org.scalatest._
import scala.concurrent.Future

class V5Spec extends AsyncFlatSpec with Matchers {

  "The V5 object" should "be valid" in {

    import V5App._

    val id = "123"
    for {
      _ <- Future.successful{}
      event1 = Turtle.create(id).right.getOrElse(sys.error("INVALID"))
      _ <- persist(event1)

      state1 <- hydrate[Turtle]("123").map(_.getOrElse(sys.error("INVALID")))
      event2 = Turtle.look(id, South).right.getOrElse(sys.error("INVALID"))
      _ <- persist(event2)

      event3 = Turtle.forward(id, 2).right.getOrElse(sys.error("INVALID"))
      _ <- persist(event3)

      state2 <- hydrate[Turtle]("123")
    } yield {
      state2 shouldBe Some(Turtle("123", 0, -2, South))
    }

  }

}
