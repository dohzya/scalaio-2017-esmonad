package esmonad

import scala.language.higherKinds

import cats.data.ReaderWriterStateT
import cats.instances.either._
import cats.instances.vector._

/**
  * Rewriting Sourced as an alias to the corresponding type class.
  */
trait V6Sourced { self: FinalHandlers =>

  case class Sourced[STATE, EVENT, A](
    run: ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], Option[STATE], A]
  ) {

    def events(initialState: Option[STATE]): Either[String, Vector[EVENT]] =
      run.runL((), initialState)

    def state(initialState: Option[STATE]): Either[String, Option[STATE]] =
      run.runS((), initialState)

    def map[B](fn: A => B): Sourced[STATE, EVENT, B] =
      Sourced(run.map(fn))

    def flatMap[B](fn: A => Sourced[STATE, EVENT, B]): Sourced[STATE, EVENT, B] =
      Sourced(run.flatMap(fn(_).run))

  }

  object Sourced {

    def sourceNew[STATE]: sourcePartiallyApplied[STATE] = new sourcePartiallyApplied[STATE]

    final class sourcePartiallyApplied[STATE] {
      def apply[EVENT](
        block: Either[String, EVENT]
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): Sourced[STATE, EVENT, Unit] =
        Sourced(for {
          _ <- ReaderWriterStateT.setF[Either[String, ?], Unit, Vector[EVENT], Option[STATE]](
            block.map { event => handler(None, event) }
          )
          _ <- ReaderWriterStateT.tellF[Either[String, ?], Unit, Vector[EVENT], Option[STATE]](
            block.map(Vector(_))
          )
        } yield ())
    }

    private def sourceInt[STATE, EVENT](
      block: STATE => Either[String, EVENT],
      error: => String,
    )(
      implicit handler: EventHandler[STATE, EVENT]
    ): ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], Option[STATE], Unit] =
      ReaderWriterStateT[Either[String, ?], Unit, Vector[EVENT], Option[STATE], Unit] {
        case ((), Some(state)) => block(state).map { event =>
          (Vector(event), handler(Some(state), event), ())
        }
        case (_, None) => Left(error)
      }

    def source[STATE, EVENT](
      block: STATE => Either[String, EVENT],
      error: => String,
    )(
      implicit handler: EventHandler[STATE, EVENT]
    ): Sourced[STATE, EVENT, Unit] = Sourced(sourceInt(block, error))

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT],
      error: => String,
    )(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT, Unit] =
      Sourced(for {
        _ <- ReaderWriterStateT.set[Either[String, ?], Unit, Vector[EVENT], Option[STATE]](Some(state))
        _ <- sourceInt(block, error)
      } yield ())
  }

}

object V6 extends FinalModels with V6Sourced
