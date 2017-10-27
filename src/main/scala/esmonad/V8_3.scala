package esmonad

import scala.language.higherKinds

import cats.data.ReaderWriterStateT
import cats.instances.either._
import cats.instances.vector._

/**
  * Rewriting Sourced as an alias to the corresponding type class.
  */
trait V8_3Sourced { self: FinalHandlers =>

  case class SourcedCreationT[STATE, EVENT, A](
    run: Either[String, (EVENT, STATE)],
    updates: SourcedUpdateT[STATE, EVENT, A],
  ) {

    def events: Either[String, Vector[EVENT]] = {
      run.flatMap { case (evt, state) =>
        updates.events(state).map { evts =>
          evt +: evts
        }
      }
    }

    def andThen[B](block: A => SourcedUpdateT[STATE, EVENT, B]): SourcedCreationT[STATE, EVENT, B] = {
      SourcedCreationT[STATE, EVENT, B](run, updates.flatMap(block))
    }

  }

  case class SourcedUpdateT[STATE, EVENT, A](run: Sourced.Impl[STATE, EVENT, A]) {

    def events(state: STATE): Either[String, Vector[EVENT]] =
      this.run.run((), state).map { case (evts, _, _) => evts }

    def map[B](fn: A => B): SourcedUpdateT[STATE, EVENT, B] =
      SourcedUpdateT(run.map(fn))

    def flatMap[B](fn: A => SourcedUpdateT[STATE, EVENT, B]): SourcedUpdateT[STATE, EVENT, B] =
      SourcedUpdateT(run.flatMap(fn(_).run))

  }

  type SourcedCreation[STATE, EVENT, A] = SourcedCreationT[STATE, EVENT, A]
  type SourcedUpdate[STATE, EVENT, A] = SourcedUpdateT[STATE, EVENT, A]

  object Sourced {

    type Impl[STATE, EVENT, A] = ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], STATE, A]

    def sourceNew[STATE]: sourcePartiallyApplied[STATE] = new sourcePartiallyApplied[STATE]

    final class sourcePartiallyApplied[STATE] {
      def apply[EVENT](
        block: Either[String, EVENT],
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): SourcedCreation[STATE, EVENT, Unit] = {
        SourcedCreationT[STATE, EVENT, Unit](
          block.map(event => event -> handler(None, event).value),
          SourcedUpdateT(ReaderWriterStateT.pure[Either[String, ?], Unit, Vector[EVENT], STATE, Unit](()))
        )
      }
    }

    private def sourceIntern[STATE, EVENT](
      block: STATE => Either[String, EVENT],
    )(
      implicit handler: EventHandler[STATE, EVENT]
    ): ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], STATE, Unit] =
      ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], STATE, Unit] {
        case ((), state) => block(state).map { event =>
          (Vector(event), handler(Some(state), event).value, ())
        }
      }

    def source[STATE, EVENT](
      block: STATE => Either[String, EVENT],
    )(
      implicit handler: EventHandler[STATE, EVENT]
    ): SourcedUpdate[STATE, EVENT, Unit] = SourcedUpdateT(sourceIntern(block))

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT],
    )(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, EVENT, Unit] =
      SourcedUpdateT(for {
        _ <- ReaderWriterStateT.set[Either[String, ?], Unit, Vector[EVENT], STATE](state)
        _ <- sourceIntern(block)
      } yield ())
  }

}

object V8_3 extends FinalModels with V8_3Sourced
