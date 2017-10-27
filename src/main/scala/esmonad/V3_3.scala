package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// Complete Event Sourced Model
trait V3_3Helpers {

  def many[STATE, EVENT](
    state: STATE, fns: Seq[STATE => Either[String, EVENT]], events: Seq[EVENT]=Vector.empty
  )(
    implicit handler: (Some[STATE], EVENT) => Some[STATE]
  ): Either[String, (Seq[EVENT], STATE)] = fns match {
    case Seq() => Right(events -> state)
    case fn +: rest =>
      fn(state).flatMap { event =>
        val Some(newState) = handler(Some(state), event)
        many(newState, rest, events :+ event)
      }
  }

}

object V3_3 extends V3_3Helpers with FinalModels
