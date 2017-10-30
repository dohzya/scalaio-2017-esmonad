package esmonad

import scala.language.higherKinds

import cats.data.ReaderWriterStateT
import cats.instances.either._
import cats.instances.vector._

/**
  * Rewriting Sourced as an alias to the corresponding type class.
  */
trait V8Sourced { self: FinalHandlers =>

  case class SourcedT[STATE, EVENT, A](run: SourcedT.Impl[STATE, EVENT, A]) {

    def events(initialState: Option[STATE])(implicit handler: EventHandler[STATE, EVENT]): Either[String, Vector[EVENT]] =
      run.runL(handler, initialState)

    def state(initialState: Option[STATE])(implicit handler: EventHandler[STATE, EVENT]): Either[String, Option[STATE]] =
      run.runS(handler, initialState)

    def map[B](fn: A => B): SourcedT[STATE, EVENT, B] =
      SourcedT(run.map(fn))

    def flatMap[B](fn: A => SourcedT[STATE, EVENT, B]): SourcedT[STATE, EVENT, B] =
      SourcedT(run.flatMap(fn(_).run))

  }

  object SourcedT {
    type Impl[STATE, EVENT, A] = ReaderWriterStateT[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE], A]
  }

  type Sourced[STATE, EVENT, A] = SourcedT[STATE, EVENT, A]

  object Sourced {

    def sourceNew[STATE]: sourcePartiallyApplied[STATE] = new sourcePartiallyApplied[STATE]

    final class sourcePartiallyApplied[STATE] {
      def apply[EVENT](
        block: Either[String, EVENT]
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): Sourced[STATE, EVENT, Unit] =
        SourcedT(for {
          _ <- ReaderWriterStateT.setF[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE]](
            block.map { event => handler(None, event) }
          )
          _ <- ReaderWriterStateT.tellF[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE]](
            block.map(Vector(_))
          )
        } yield ())
    }

    private def sourceInt[STATE, EVENT](
      block: STATE => Either[String, EVENT],
      error: => String,
    ): ReaderWriterStateT[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE], Unit] =
      ReaderWriterStateT[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE], Unit] {
        case (handler, Some(state)) => block(state).map { event =>
          (Vector(event), handler(Some(state), event), ())
        }
        case (_, None) => Left(error)
      }

    def source[STATE, EVENT](
      block: STATE => Either[String, EVENT],
      error: => String,
    ): Sourced[STATE, EVENT, Unit] = SourcedT(sourceInt(block, error))

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT],
      error: => String,
    ): Sourced[STATE, EVENT, Unit] =
      SourcedT(for {
        _ <- ReaderWriterStateT.set[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE]](Some(state))
        _ <- sourceInt(block, error)
      } yield ())

    def when[STATE] = new WhenPartiallyApplied[STATE]
    final class WhenPartiallyApplied[STATE] {
      def apply[EVENT](
        test: STATE => Boolean,
        block: STATE => Either[String, EVENT],
        error: => String,
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): Sourced[STATE, EVENT, Unit] =
        SourcedT(ReaderWriterStateT[Either[String, ?], EventHandler[STATE, EVENT], Vector[EVENT], Option[STATE], Unit] {
          case (handler, Some(state)) if test(state) => block(state).map { event =>
            (Vector(event), handler(Some(state), event), ())
          }
          case (_, Some(state)) =>
            Right((Vector.empty, Some(state), ()))
          case (_, None) => Left(error)
        })
    }
  }

}

object V8 extends FinalModels with V8Sourced
