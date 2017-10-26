package esmonad

import cats.{FlatMap, Functor}
import cats.data.WriterT
import cats.instances.vector._

import scala.language.higherKinds

/**
 * Rewriting Sourced as an alias to the corresponding type class.
 */
trait V6_3Sourced { self: FinalHandlers =>

  case class SourcedT[F[_], STATE, EVENT](run: SourcedT.Impl[F, STATE, EVENT]) {

    def events(implicit F: Functor[F]): F[Vector[EVENT]] = run.written

    def map[B](fn: STATE => B)(implicit F: Functor[F]): SourcedT[F, B, EVENT] =
      SourcedT(run.map(fn))

    def flatMap[B](fn: STATE => SourcedT[F, B, EVENT])(implicit F: FlatMap[F]): SourcedT[F, B, EVENT] =
      SourcedT(run.flatMap(fn(_).run))

  }

  object SourcedT {
    type Impl[F[_], STATE, EVENT] = WriterT[F, Vector[EVENT], STATE]
  }

  type Sourced[STATE, EVENT] = SourcedT[Either[String, ?], STATE, EVENT]

  object Sourced {

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
        SourcedT(WriterT[Either[String, ?], Vector[EVENT], STATE](
          block.map { event =>
            Vector(event) -> handler(None, event).value
          }
        ))
    }

    def source[STATE, EVENT](
      block: STATE => Either[String, EVENT]
    )(implicit handler: EventHandler[STATE, EVENT]): STATE => Sourced[STATE, EVENT] =
      state => new Sourced(WriterT[Either[String, ?], Vector[EVENT], STATE](
        block(state).map { event =>
          Vector(event) -> handler(Some(state), event).value
        }
      ))

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT]
    )(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
      new Sourced(WriterT[Either[String, ?], Vector[EVENT], STATE](
        block(state).map { event =>
          Vector(event) -> handler(Some(state), event).value
        }
      ))

  }

}

object V6_3 extends FinalModels with V6_3Sourced
