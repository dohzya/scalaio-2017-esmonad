package esmonad

import scala.language.higherKinds

import cats.data.ReaderWriterStateT
import cats.instances.either._
import cats.instances.vector._

/**
  * Rewriting Sourced as an alias to the corresponding type class.
  */
trait V6_2Sourced { self: FinalHandlers =>

  case class SourcedCreation[STATE, EVENT, A](
    run: Either[String, (EVENT, STATE)],
    updates: SourcedUpdate[STATE, EVENT, A],
  ) {

    def events: Either[String, Vector[EVENT]] = {
      run.flatMap { case (evt, state) =>
        updates.events(state).map { evts =>
          evt +: evts
        }
      }
    }

    def state: Either[String, STATE] = {
      run.flatMap { case (evt, state) =>
        updates.state(state)
      }
    }

    def andThen[B](block: A => SourcedUpdate[STATE, EVENT, B]): SourcedCreation[STATE, EVENT, B] = {
      SourcedCreation[STATE, EVENT, B](run, updates.flatMap(block))
    }

  }

  case class SourcedUpdate[STATE, EVENT, A](
    run: ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], STATE, A]
  ) {

    def events(initialState: STATE): Either[String, Vector[EVENT]] =
      this.run.runL((), initialState)

    def state(initialState: STATE): Either[String, STATE] =
      this.run.runS((), initialState)

    def map[B](fn: A => B): SourcedUpdate[STATE, EVENT, B] =
      SourcedUpdate(run.map(fn))

    def flatMap[B](fn: A => SourcedUpdate[STATE, EVENT, B]): SourcedUpdate[STATE, EVENT, B] =
      SourcedUpdate(run.flatMap(fn(_).run))

  }

  object Sourced {

    def sourceNew[STATE]: sourcePartiallyApplied[STATE] = new sourcePartiallyApplied[STATE]

    final class sourcePartiallyApplied[STATE] {
      def apply[EVENT](
        block: Either[String, EVENT],
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): SourcedCreation[STATE, EVENT, Unit] = {
        SourcedCreation[STATE, EVENT, Unit](
          block.map(event => event -> handler(None, event).value),
          SourcedUpdate(ReaderWriterStateT.pure[Either[String, ?], Unit, Vector[EVENT], STATE, Unit](()))
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
    ): SourcedUpdate[STATE, EVENT, Unit] = SourcedUpdate(sourceIntern(block))

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT],
    )(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, EVENT, Unit] =
      SourcedUpdate(for {
        _ <- ReaderWriterStateT.set[Either[String, ?], Unit, Vector[EVENT], STATE](state)
        _ <- sourceIntern(block)
      } yield ())

    def when[STATE] = new WhenPartiallyApplied[STATE]
    final class WhenPartiallyApplied[STATE] {
      def apply[EVENT](
        test: STATE => Boolean,
        block: STATE => Either[String, EVENT],
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): SourcedUpdate[STATE, EVENT, Unit] =
        SourcedUpdate(ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], STATE, Unit] {
          case ((), state) if test(state) =>
            block(state).map { event =>
              (Vector(event), handler(Some(state), event).value, ())
            }
          case ((), state)  => Right(Vector.empty, state, ())
        })
    }

  }

}

object V6_2 extends FinalModels with V6_2Sourced
