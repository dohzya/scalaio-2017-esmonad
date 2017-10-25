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

    def andThen(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: FlatMap[F]): SourcedCreationT[F, STATE, EVENT] = {
      SourcedCreationT(this.impl.flatMap(other.impl.run))
    }
  }

  object SourcedCreationT {
    type Impl[F[_], STATE, EVENT, A] = WriterT[F, Vector[EVENT], A]
  }

  case class SourcedUpdateT[F[_], STATE, EVENT](
    impl: SourcedUpdateT.Impl[F, STATE, EVENT]
  ) {
    def run(state: STATE): F[(Vector[EVENT], STATE)] = {
      impl.run(state).run
    }

    def events(state: STATE)(implicit F: Functor[F]): F[Vector[EVENT]] = {
      impl.run(state).written
    }

    def andThen(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: FlatMap[F]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT(this.impl.andThen(other.impl))
    }
  }

  object SourcedUpdateT {
    type Impl[F[_], STATE, EVENT] = Kleisli[WriterT[F, Vector[EVENT], ?], STATE, STATE]
  }

  object Sourced {

    def events[F[_], STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdateT[F, STATE, EVENT])(implicit F: Functor[F]): F[Vector[EVENT]] = {
      sourcedUpdate.events(state)
    }

    final class SourceNewPartiallyApplied[STATE] {
      def apply[F[_], EVENT](block: F[EVENT])(implicit F: Functor[F], handler: EventHandler[STATE, EVENT]): SourcedCreationT[F, STATE, EVENT] = {
        SourcedCreationT[F, STATE, EVENT] {
            WriterT(
              block.map { event =>
                (Vector(event), handler(None, event).value)
              }
            )
        }
      }
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    def source[F[_], STATE, EVENT](block: STATE => F[EVENT])(implicit F: Functor[F], handler: EventHandler[STATE, EVENT]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT[F, STATE, EVENT] {
        Kleisli { state =>
          WriterT {
            block(state).map { event =>
              (Vector(event), handler(Some(state), event).value)
            }
          }
        }
      }
    }
  }
}

object V7_3 extends V7_3Sourced with V4Handler with V5Journal with V5Models
