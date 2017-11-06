package esmonad

import cats.data.{Kleisli, WriterT}
import cats.instances.either._
import cats.instances.vector._

import scala.language.higherKinds

/**
 * Rewriting SourcedCreationT and SourcedCreationT as aliases to the corresponding type classes.
 */
trait V5_2Sourced { self: FinalHandlers =>

  case class SourcedCreation[STATE, EVENT](
    impl: WriterT[Either[String, ?], Vector[EVENT], STATE]
  ) {
    def run: Either[String, (Vector[EVENT], STATE)] = {
      impl.run
    }

    def events: Either[String, Vector[EVENT]] = {
      impl.written
    }

    def andThen[A](other: SourcedUpdate[STATE, EVENT, A]): SourcedCreation[A, EVENT] = {
      SourcedCreation(this.impl.flatMap(other.impl.run))
    }
  }

  case class SourcedUpdate[STATE, EVENT, A](
    impl: Kleisli[WriterT[Either[String, ?], Vector[EVENT], ?], STATE, A]
  ) {
    def run(state: STATE): Either[String, (Vector[EVENT], A)] = {
      impl.run(state).run
    }

    def events(state: STATE): Either[String, Vector[EVENT]] = {
      impl.run(state).written
    }

    def andThen[B](other: SourcedUpdate[A, EVENT, B]): SourcedUpdate[STATE, EVENT, B] = {
      SourcedUpdate(this.impl.andThen(other.impl))
    }
  }

  object Sourced {

    def events[STATE, EVENT](sourcedCreationT: SourcedCreation[STATE, EVENT]): Either[String, Vector[EVENT]] = {
      sourcedCreationT.events
    }

    def events[STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdate[STATE, EVENT, _]): Either[String, Vector[EVENT]] = {
      sourcedUpdate.events(state)
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedCreation[STATE, EVENT] = {
        SourcedCreation[STATE, EVENT] {
            WriterT[Either[String, ?], Vector[EVENT], STATE](
              block.map { event =>
                Vector(event) -> handler(None, event).value
              }
            )
        }
      }
    }

    def source[STATE, EVENT](block: STATE => Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, EVENT, STATE] = {
      SourcedUpdate[STATE, EVENT, STATE] {
        Kleisli { state =>
          WriterT[Either[String, ?], Vector[EVENT], STATE] {
            block(state).map { event =>
              Vector(event) -> handler(Some(state), event).value
            }
          }
        }
      }
    }

    def when[STATE] = new whenPartiallyApplied[STATE]
    final class whenPartiallyApplied[STATE] {
      def apply[EVENT](
        predicate: (STATE) => Boolean,
        block: STATE => Either[String, EVENT]
      )(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, EVENT, STATE] =
        SourcedUpdate[STATE, EVENT, STATE] {
          Kleisli { state =>
            if (predicate(state)) {
              WriterT[Either[String, ?], Vector[EVENT], STATE] {
                block(state).map { event =>
                  Vector(event) -> handler(Some(state), event).value
                }
              }
            } else {
              WriterT[Either[String, ?], Vector[EVENT], STATE] {
                Right(Vector.empty -> state)
              }
            }
          }
        }
    }


  }
}

object V5_2 extends FinalModels with V5_2Sourced
