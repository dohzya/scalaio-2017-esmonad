Plan
====

## 0

```scala
val state1 = Turtle.look(state1, South)
val state2 = Turtle.forward(state2, 2)
val state3 = Turtle.look(state3, West)
```

## Tests

```scala
package esmonad

import scala.concurrent.Future

import cats.data.{EitherT, OptionT}
import esmonad.V6App.{hydrate, persist}

class Spec {


  for {

    first <- demiTour()
  }

  (
    for {
      _ <- EitherT.fromEither[Future](Right(()))

      (_, event1) = Turtle.create("123")
      _ <- EitherT.right(persist(event1))

      state1: Turtle <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      sourcedState: Sourced[Turtle] <- EitherT.right(for{
        state2 <- Turtle.look(state1, South)
        state3 <- Turtle.forward(state2, 2)
        state4 <- Turtle.look(state3, West)
      } yield state4)

      val LTurn = Sourced[Turtle] {
        for {
          _ <- act(Turtle.look(South))
          _ <- act(Turtle.forward(2))
          _ <- act(Turtle.look(West))
        } yield ()
      }


      import syntax._
      LTurn.run(state1, handler)
      Sourced { b =>
        for {
          _ <- act(Turtle.forward(2))(b) // L-Turn
          _ <- when(predicate: STATE => Boolean) {
            for {
              _ <- LTurn
              _ <- LTurn
            } yield ()
          }(b)
        } yield ()
      }.run(state1, handler)

      import syntax._
      LTurn.run(state1, handler)
      Sourced { implicit b =>
        for {
          _ <- act(Turtle.forward(2)) // L-Turn
          _ <- when(predicate: STATE => Boolean) {
            for {
              _ <- LTurn
              _ <- LTurn
            } yield ()
          }
        } yield ()
      }.run(state1, handler)

      val sourcedState1 = Sourced(state1, handler) { implicit xxx =>
        act(Turtle.forward(2)) and
        act(Turtle.look(South)) and
        act(Turtle.forward(2)) and
        LTurn
      }
      val sourcedState2 = ???

      persist(Sourced.zip(sourcedState1, sourcedState2))
      persist(Seq(sourcedState1, sourcedState2))

      sourcedState2: Sourced[Turtle] <- EitherT.right(for{
        _ <- write(state1) // State[Unit, Sourced[Turtle]] but stacked

        _ <- act(Turtle.look(_, South))
        _ <- act(Turtle.forward(_, 2))
        _ <- act(Turtle.look(_, West))

        _ <- write(state1) // State[Unit, Sourced[Turtle]] but stacked

        _ <- act(Turtle.forward(2)) // L-Turn
        _ <- act(Turtle.look(South))
        _ <- act(Turtle.forward(2))
        _ <- act(Turtle.look(West))
        _ <- act(Turtle.forward(2))

        _ <- lTurn
        _ <- lTurn

        _ <- act(Turtle.look(_, West))
        state2 <- read()
      } yield state2)
      persistedState <- EitherT.right(persist(sourcedState))

      state4Prime <- OptionT(hydrate[Turtle]("123")).toRight("not found")
    } yield (state4, state4Prime)
    ).value.map {
    case (beforePertistence, afterPersistance) =>
      afterPersistance shouldBe beforePertistence
      afterPersistance shouldBe Right(Turtle("123", 0, -2, West))
  }

}
```
