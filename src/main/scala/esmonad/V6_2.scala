package esmonad

import cats.{FlatMap, Functor}
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.language.higherKinds

/**
 * Rewriting Sourced as a monad transformer to abstract validation.
 */
trait V6_2Sourced { self: FinalHandlers =>

  case class SourcedT[F[_], STATE, EVENT](run: F[(Vector[EVENT], STATE)]) {

    def events(implicit F: Functor[F]): F[Vector[EVENT]] = run.map { case (events, _) => events }

    def map[B](fn: STATE => B)(implicit F: Functor[F]): SourcedT[F, B, EVENT] =
      SourcedT[F, B, EVENT](run.map { case (events, state) =>
        (events, fn(state))
      })

    def flatMap[B](fn: STATE => SourcedT[F, B, EVENT])(implicit F: FlatMap[F]): SourcedT[F, B, EVENT] =
      SourcedT(run.flatMap { case (oldEvents, oldState) =>
        fn(oldState).run.map { case (newEVents, newState) =>
          (oldEvents ++ newEVents, newState)
        }
      })

  }

  type Sourced[STATE, EVENT] = SourcedT[Either[String, ?], STATE, EVENT]

  object Sourced {

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
        new Sourced(block.map { e =>
          val Some(newState) = handler(None, e)
          Vector(e) -> newState
        })
    }

    def source[STATE, EVENT](
      block: STATE => Either[String, EVENT]
    )(implicit handler: EventHandler[STATE, EVENT]): STATE => Sourced[STATE, EVENT] =
      state => new Sourced(block(state).map { e =>
        val Some(newState) = handler(Some(state), e)
        Vector(e) -> newState
      })

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT]
    )(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
      new Sourced(block(state).map { e =>
        val Some(newState) = handler(Some(state), e)
        Vector(e) -> newState
      })

  }

}

object V6_2 extends FinalModels with V6_2Sourced
