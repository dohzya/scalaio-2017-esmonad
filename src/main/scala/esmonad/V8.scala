package esmonad

import cats.data.{Kleisli, WriterT}
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Functor, Monad}

import scala.language.higherKinds

object V8App extends V8 with App

/**
 * In this version, we abstract over the type of errors, side effects etc in the event handlers
 */
trait V8 extends V6Handler with V6Journal with V6Model {


  case class SourcedCreationT[F[_], STATE, EVENT](
    run: (EventHandler[STATE, EVENT]) => F[(STATE, Seq[EVENT])]
  ) {

    def events(handler: EventHandler[STATE, EVENT])(implicit F: Functor[F]): F[Seq[EVENT]] = run(handler).map(_._2)

    def and(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: FlatMap[F]): SourcedCreationT[F, STATE, EVENT] = {
      SourcedCreationT[F, STATE, EVENT] {
        (handler) =>
          this.run(handler).flatMap { case (thisState, thisEvents) =>
            other.run(thisState, handler).map { case (otherState, otherEvents) =>
              (otherState, thisEvents ++ otherEvents)
            }
          }
      }
    }
  }

  case class SourcedUpdateT[F[_], STATE, EVENT](
    run: (STATE, EventHandler[STATE, EVENT]) => F[(STATE, Seq[EVENT])]
  ) {

    def events(state: STATE, handler: EventHandler[STATE, EVENT])(implicit F: Functor[F]): F[Seq[EVENT]] = {
      run(state, handler).map(_._2)
    }

    def and(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: FlatMap[F]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT[F, STATE, EVENT] {
        (initialState, handler) =>
          this.run(initialState, handler).flatMap { case (thisState, thisEvents) =>
            other.run(thisState, handler).map { case (otherState, otherEvents) =>
              (otherState, thisEvents ++ otherEvents)
            }
          }
      }
    }
  }


  object Sourced {

    def events[F[_], STATE, EVENT](state: STATE, handler: EventHandler[STATE, EVENT])(sourcedUpdate: SourcedUpdateT[F, STATE, EVENT])(implicit F: Functor[F]): F[Seq[EVENT]] = {
      sourcedUpdate.events(state, handler)
    }

    def events[F[_], STATE, EVENT](handler: EventHandler[STATE, EVENT])(sourcedCreation: SourcedCreationT[F, STATE, EVENT])(implicit F: Functor[F]): F[Seq[EVENT]] = {
      sourcedCreation.events(handler)
    }


    final class SourceNewPartiallyApplied[STATE] {
      def apply[F[_], EVENT](block: F[EVENT])(implicit F: Functor[F]): SourcedCreationT[F, STATE, EVENT] = {
        SourcedCreationT[F, STATE, EVENT] {
          handler =>
            block.map { event =>
              (handler(None, event).value, Seq(event))
            }
        }
      }
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    def source[F[_], STATE, EVENT](block: STATE => F[EVENT])(implicit F: Functor[F]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT[F, STATE, EVENT] {
        (state, handler) =>
          block(state).map { event =>
            (handler(Some(state), event).value, Seq(event))
          }
      }
    }
  }

}

object V8TypeClasses extends V8TypeClasses

// The same, but written with the corresponding typeclasses
trait V8TypeClasses extends V6Handler with V6Journal {

  case class SourcedCreationT[F[_], STATE, EVENT](
    impl: SourcedCreationT.Impl[F, STATE, EVENT, STATE]
  ) {
    def run(handler: EventHandler[STATE, EVENT]): F[(List[EVENT], STATE)] = {
      impl.run(handler).run
    }

    def events(handler: EventHandler[STATE, EVENT])(implicit F: Functor[F]): F[List[EVENT]] = {
      impl.run(handler).written
    }

    def and(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: Monad[F]): SourcedCreationT[F, STATE, EVENT] = {
      SourcedCreationT(this.impl.flatMap(other.impl.run))
    }
  }

  object SourcedCreationT {
    type Impl[F[_], STATE, EVENT, A] = Kleisli[WriterT[F, List[EVENT], ?], EventHandler[STATE, EVENT], A]
  }

  case class SourcedUpdateT[F[_], STATE, EVENT](
    impl: SourcedUpdateT.Impl[F, STATE, EVENT]
  ) {
    def run(state: STATE, handler: EventHandler[STATE, EVENT]): F[(List[EVENT], STATE)] = {
      impl.run(state).run(handler).run
    }

    def events(state: STATE, handler: EventHandler[STATE, EVENT])(implicit F: Functor[F]): F[List[EVENT]] = {
      impl.run(state).run(handler).written
    }

    def and(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: Monad[F]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT(this.impl.flatMapF(other.impl.run))
    }
  }

  object SourcedUpdateT {
    type Impl[F[_], STATE, EVENT] = Kleisli[Kleisli[WriterT[F, List[EVENT], ?], EventHandler[STATE, EVENT], ?], STATE, STATE]
  }

  object Sourced {

    def events[F[_], STATE, EVENT](state: STATE, handler: EventHandler[STATE, EVENT])(sourcedUpdate: SourcedUpdateT[F, STATE, EVENT])(implicit F: Functor[F]): F[List[EVENT]] = {
      sourcedUpdate.events(state, handler)
    }

    def events[F[_], STATE, EVENT](handler: EventHandler[STATE, EVENT])(sourcedCreation: SourcedCreationT[F, STATE, EVENT])(implicit F: Functor[F]): F[List[EVENT]] = {
      sourcedCreation.events(handler)
    }


    final class SourceNewPartiallyApplied[STATE] {
      def apply[F[_], EVENT](block: F[EVENT])(implicit F: Functor[F]): SourcedCreationT[F, STATE, EVENT] = {
        SourcedCreationT[F, STATE, EVENT] {
          Kleisli { handler =>
            WriterT(
              block.map { event =>
                (List(event), handler(None, event).value)
              }
            )
          }
        }
      }
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    def source[F[_], STATE, EVENT](block: STATE => F[EVENT])(implicit F: Functor[F]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT[F, STATE, EVENT] {
        Kleisli { state =>
          Kleisli { handler =>
            WriterT(
              block(state).map { event =>
                (List(event), handler(Some(state), event).value)
              }
            )
          }
        }
      }
    }
  }
}
