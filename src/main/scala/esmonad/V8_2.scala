package esmonad

import scala.language.higherKinds

import cats.{FlatMap, Functor, Monad}
import cats.data.ReaderWriterStateT
import cats.instances.either._
import cats.instances.vector._
import cats.syntax.functor._

/**
  * Rewriting Sourced as an alias to the corresponding type class.
  */
trait V8_2Sourced { self: FinalHandlers =>

  case class SourcedT[F[_], STATE, EVENT, A](run: SourcedT.Impl[F, STATE, EVENT, A]) {

    def events(state: Option[STATE])(implicit F: Monad[F]): F[Vector[EVENT]] =
      run.written.run((), state).map { case (events, _, _) => events }

    def map[B](fn: A => B)(implicit F: Functor[F]): SourcedT[F, STATE, EVENT, B] =
      SourcedT(run.map(fn))

    def flatMap[B](fn: A => SourcedT[F, STATE, EVENT, B])(implicit F: FlatMap[F]): SourcedT[F, STATE, EVENT, B] =
      SourcedT(run.flatMap(fn(_).run))

  }

  object SourcedT {
    type Impl[F[_], STATE, EVENT, A] = ReaderWriterStateT[F, Unit, Vector[EVENT], Option[STATE], A]
  }

  type Sourced[STATE, EVENT, A] = SourcedT[Either[String, ?], STATE, EVENT, A]

  object Sourced {

    def source[STATE]: sourcePartiallyApplied[STATE] = new sourcePartiallyApplied[STATE]

    final class sourcePartiallyApplied[STATE] {
      def apply[EVENT](
        block: Either[String, EVENT]
      )(
        implicit handler: EventHandler[STATE, EVENT]
      ): Sourced[STATE, EVENT, Unit] =
        SourcedT(for {
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
    ): Sourced[STATE, EVENT, Unit] = SourcedT(sourceInt(block, error))

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT],
      error: => String,
    )(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT, Unit] =
      SourcedT(for {
        _ <- ReaderWriterStateT.set[Either[String, ?], Unit, Vector[EVENT], Option[STATE]](Some(state))
        _ <- sourceInt(block, error)
      } yield ())
  }

}

object V8_2 extends FinalModels with V8_2Sourced
