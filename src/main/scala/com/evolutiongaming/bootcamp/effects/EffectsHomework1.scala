package com.evolutiongaming.bootcamp.effects

import com.evolutiongaming.bootcamp.effects.EffectsHomework1.IO.raiseError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1 {
  final class IO[A](private val run: () => A) {
    def map[B](f: A => B): IO[B] = new IO(() => f(run()))
    def flatMap[B](f: A => IO[B]): IO[B] = f(run())
    def *>[B](another: IO[B]): IO[B] = another
    def as[B](newValue: => B): IO[B] = new IO(() => newValue)
    def void: IO[Unit] = new IO(() => ())
    def attempt: IO[Either[Throwable, A]] = run() match {
      case None => new IO(() => Left(Throwable))
      case value => new IO(() => Right(value))
    }
    def option: IO[Option[A]] = new IO(() => Option(run()))
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = run()
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = ???
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = ???
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = Future{run()}
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)
    def suspend[A](thunk: => IO[A]): IO[A] = thunk
    def delay[A](body: => A): IO[A] = apply(body)
    def pure[A](a: A): IO[A] = IO(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(error)  => raiseError(error)
      case Right(value) => pure(value)
    }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None        => raiseError(orElse)
      case Some(value) => pure(value)
    }
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(cause) => raiseError(cause)
      case Success(value) => pure(value)
    }
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = new IO(() => throw e)

    // I think this is a bullshit code, down of that comment. Advise is needed :)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = cond match {
      case false => raiseUnless(cond)(e) // <- here and below recursive call but condition will never be changed
      case true  => unit
    }
    def raiseUnless1(cond: Boolean)(e: => Throwable): IO[Unit] = cond match {
      case false => raiseError(e) // another solution, but in that case we haven't "unless", we just have one iteration
      case true  => unit
    }
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = cond match {
      case false => unit
      case true  => raiseWhen(cond)(e)
    }
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = cond match {
      case false => unlessA(cond)(action)
      case true  => action
    }
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = cond match {
      case false => action
      case true  => whenA(cond)(action)
    }
    val unit: IO[Unit] = new IO(() => ())
  }
}
