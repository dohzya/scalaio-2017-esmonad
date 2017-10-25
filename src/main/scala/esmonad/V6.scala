package esmonad

import cats.{FlatMap, Functor}
import cats.instances.either._
import cats.syntax.functor._
import cats.syntax.flatMap._

trait V6Sourced { self: V4Handler =>

  case class SourcedT[F[_], STATE, EVENT](run: F[(Vector[EVENT], STATE)]) {

    def events(implicit F: Functor[F]): F[Vector[EVENT]] = run.map { case (events, _) => events }

    def map[B](f: STATE => B)(implicit F: Functor[F]): SourcedT[F, B, EVENT] = {
      SourcedT[F, B, EVENT](run.map { case (events, state) =>
        (events, f(state))
      })
    }

    def flatMap[B](f: STATE => SourcedT[F, B, EVENT])(implicit F: FlatMap[F]): SourcedT[F, B, EVENT] = {
      SourcedT(run.flatMap { case (oldEvents, oldState) =>
        f(oldState).run.map { case (newEVents, newState) =>
          (oldEvents ++ newEVents, newState)
        }
      })
    }

  }

  //type SourcedT[F[_], STATE, EVENT] = WriterT[F, Vector[EVENT], STATE]

  type Sourced[STATE, EVENT] = SourcedT[Either[String, ?], STATE, EVENT]

  object Sourced {

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

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
        new Sourced(block.map { e =>
          val Some(newState) = handler(None, e)
          Vector(e) -> newState
        })
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]
  }

}

object V6 extends V6Sourced with V5Models with V4Handler with V5Journal
