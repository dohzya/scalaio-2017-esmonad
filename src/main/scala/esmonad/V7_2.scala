package esmonad

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Functor}

import scala.language.higherKinds

/**
 * Rewriting SourcedCreationT and SourcedCreationT as monad transformers.
 */
trait V7_2Sourced { self: FinalHandlers =>

  case class SourcedCreationT[F[_], STATE, EVENT](
    run: F[(Seq[EVENT], STATE)]
  ) {

    def events(implicit F: Functor[F]): F[Seq[EVENT]] = run.map(_._1)

    def andThen(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: FlatMap[F]): SourcedCreationT[F, STATE, EVENT] = {
      SourcedCreationT[F, STATE, EVENT] {
        this.run.flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }

  case class SourcedUpdateT[F[_], STATE, EVENT](
    run: (STATE) => F[(Seq[EVENT], STATE)]
  ) {

    def events(state: STATE)(implicit F: Functor[F]): F[Seq[EVENT]] = {
      run(state).map(_._1)
    }

    def andThen(other: SourcedUpdateT[F, STATE, EVENT])(implicit F: FlatMap[F]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT[F, STATE, EVENT] { initialState =>
        this.run(initialState).flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }


  object Sourced {

    def events[F[_], STATE, EVENT](sourcedCreationT: SourcedCreationT[F, STATE, EVENT])(implicit F: Functor[F]): F[Seq[EVENT]] = {
      sourcedCreationT.events
    }

    def events[F[_], STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdateT[F, STATE, EVENT])(implicit F: Functor[F]): F[Seq[EVENT]] = {
      sourcedUpdate.events(state)
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[F[_], EVENT](block: F[EVENT])(implicit F: Functor[F], handler: EventHandler[STATE, EVENT]): SourcedCreationT[F, STATE, EVENT] = {
        SourcedCreationT[F, STATE, EVENT] {
          block.map { event =>
            (Seq(event), handler(None, event).value)
          }
        }
      }
    }

    def source[F[_], STATE, EVENT](block: STATE => F[EVENT])(implicit F: Functor[F], handler: EventHandler[STATE, EVENT]): SourcedUpdateT[F, STATE, EVENT] = {
      SourcedUpdateT[F, STATE, EVENT] { state =>
        block(state).map { event =>
          (Seq(event), handler(Some(state), event).value)
        }
      }
    }
  }

}

object V7_2 extends FinalModels with V7_2Sourced
