package esmonad

import cats.data.{Kleisli, WriterT}
import cats.instances.vector._
import cats.syntax.functor._
import cats.{FlatMap, Functor}

import scala.language.higherKinds

// The same, but written with the corresponding typeclasses
trait V7_3Sourced { self: V4Handler =>

  case class SourcedCreationT[F[_], STATE, EVENT](
    impl: SourcedCreationT.Impl[F, STATE, EVENT, STATE]
  ) {
    def run: F[(Vector[EVENT], STATE)] = {
      impl.run
    }

    def events(implicit F: Functor[F]): F[Vector[EVENT]] = {
      impl.written
    }

    def andThen[A](other: SourcedUpdateT[F, STATE, EVENT, A])(implicit F: FlatMap[F]): SourcedCreationT[F, A, EVENT] = {
      SourcedCreationT(this.impl.flatMap(other.impl.run))
    }
  }

  object SourcedCreationT {
    type Impl[F[_], STATE, EVENT, A] = WriterT[F, Vector[EVENT], A]
  }

  case class SourcedUpdateT[F[_], STATE, EVENT, A](
    impl: SourcedUpdateT.Impl[F, STATE, EVENT, A]
  ) {
    def run(state: STATE): F[(Vector[EVENT], STATE)] = {
      impl.run(state).run
    }

    def events(state: STATE)(implicit F: Functor[F]): F[Vector[EVENT]] = {
      impl.run(state).written
    }

    def andThen[B](other: SourcedUpdateT[F, A, EVENT, B])(implicit F: FlatMap[F]): SourcedUpdateT[F, STATE, EVENT, B] = {
      SourcedUpdateT(this.impl.andThen(other.impl))
    }
  }

  object SourcedUpdateT {
    type Impl[F[_], STATE, EVENT, A] = Kleisli[WriterT[F, Vector[EVENT], ?], STATE, A]
  }

  object Sourced {

    def events[F[_], STATE, EVENT](sourcedCreationT: SourcedCreationT[F, STATE, EVENT])(implicit F: Functor[F]): F[Vector[EVENT]] = {
      sourcedCreationT.events
    }

    def events[F[_], STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdateT[F, STATE, EVENT, _])(implicit F: Functor[F]): F[Vector[EVENT]] = {
      sourcedUpdate.events(state)
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[F[_], EVENT](block: F[EVENT])(implicit F: Functor[F], handler: EventHandler[STATE, EVENT]): SourcedCreationT[F, STATE, EVENT] = {
        SourcedCreationT[F, STATE, EVENT] {
            WriterT(
              block.map { event =>
                Vector(event) -> handler(None, event).value
              }
            )
        }
      }
    }

    def source[F[_], STATE, EVENT](block: STATE => F[EVENT])(implicit F: Functor[F], handler: EventHandler[STATE, EVENT]): SourcedUpdateT[F, STATE, EVENT, STATE] = {
      SourcedUpdateT[F, STATE, EVENT, STATE] {
        Kleisli { state =>
          WriterT {
            block(state).map { event =>
              Vector(event) -> handler(Some(state), event).value
            }
          }
        }
      }
    }
  }
}

object V7_3 extends V7_3Sourced with V4Handler with V5Journal with V5Models
