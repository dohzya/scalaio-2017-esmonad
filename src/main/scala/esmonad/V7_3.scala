package esmonad

import cats.data.{Kleisli, WriterT}
import cats.instances.either._
import cats.instances.vector._

import scala.language.higherKinds

/**
 * Rewriting SourcedCreationT and SourcedCreationT as aliases to the corresponding type classes.
 */
trait V7_3Sourced { self: FinalHandlers =>

  case class SourcedCreationT[STATE, EVENT](
    impl: SourcedCreationT.Impl[STATE, EVENT, STATE]
  ) {
    def run: Either[String, (Vector[EVENT], STATE)] = {
      impl.run
    }

    def events: Either[String, Vector[EVENT]] = {
      impl.written
    }

    def andThen[A](other: SourcedUpdateT[STATE, EVENT, A]): SourcedCreationT[A, EVENT] = {
      SourcedCreationT(this.impl.flatMap(other.impl.run))
    }
  }

  object SourcedCreationT {
    type Impl[STATE, EVENT, A] = WriterT[Either[String, ?], Vector[EVENT], A]
  }

  case class SourcedUpdateT[STATE, EVENT, A](
    impl: SourcedUpdateT.Impl[STATE, EVENT, A]
  ) {
    def run(state: STATE): Either[String, (Vector[EVENT], A)] = {
      impl.run(state).run
    }

    def events(state: STATE): Either[String, Vector[EVENT]] = {
      impl.run(state).written
    }

    def andThen[B](other: SourcedUpdateT[A, EVENT, B]): SourcedUpdateT[STATE, EVENT, B] = {
      SourcedUpdateT(this.impl.andThen(other.impl))
    }
  }

  object SourcedUpdateT {
    type Impl[STATE, EVENT, A] = Kleisli[WriterT[Either[String, ?], Vector[EVENT], ?], STATE, A]
  }

  object Sourced {

    def events[STATE, EVENT](sourcedCreationT: SourcedCreationT[STATE, EVENT]): Either[String, Vector[EVENT]] = {
      sourcedCreationT.events
    }

    def events[STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdateT[STATE, EVENT, _]): Either[String, Vector[EVENT]] = {
      sourcedUpdate.events(state)
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedCreationT[STATE, EVENT] = {
        SourcedCreationT[STATE, EVENT] {
            WriterT[Either[String, ?], Vector[EVENT], STATE](
              block.map { event =>
                Vector(event) -> handler(None, event).value
              }
            )
        }
      }
    }

    def source[STATE, EVENT](block: STATE => Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdateT[STATE, EVENT, STATE] = {
      SourcedUpdateT[STATE, EVENT, STATE] {
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
      )(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdateT[STATE, EVENT, STATE] =
        SourcedUpdateT[STATE, EVENT, STATE] {
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

object V7_3 extends FinalModels with V7_3Sourced
