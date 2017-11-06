package esmonad

import scala.language.higherKinds

import cats.data.WriterT
import cats.instances.either._
import cats.instances.vector._

/**
 * Rewriting Sourced as an alias to the corresponding type class.
 */
trait V4_2Sourced { self: FinalHandlers =>

  case class Sourced[STATE, EVENT](run: WriterT[Either[String, ?], Vector[EVENT], STATE]) {

    def events: Either[String, Vector[EVENT]] = run.written

    def map[B](fn: STATE => B): Sourced[B, EVENT] =
      Sourced(run.map(fn))

    def flatMap[B](fn: STATE => Sourced[B, EVENT]): Sourced[B, EVENT] =
      Sourced(run.flatMap(fn(_).run))

  }

  object Sourced {

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
        Sourced(WriterT[Either[String, ?], Vector[EVENT], STATE](
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

object V4_2 extends FinalModels with V4_2Sourced
