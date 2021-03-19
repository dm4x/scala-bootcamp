/*
  Additional materials:

  Papers by Oleg Kiselyov http://okmij.org/ftp/tagless-final/index.html
  (Haskell, 18+)

  Что такое tagless final? https://www.youtube.com/watch?v=ZNK57IXgr3M
  (Scala 3, история развития кодировок от Черча до TF)

  Tagless Final lessons series https://www.youtube.com/watch?v=XJ2NjqkWdck&list=PLJGDHERh23x-3_T3Dua6Fwp4KlG0J25DI

  Practical FP in Scala https://leanpub.com/pfp-scala (Chapters 3, 4)
*/

import cats._
import cats.syntax.all._


sealed trait Expression

final case class Const(x: Int) extends Expression
final case class Add(left: Expression, right: Expression) extends Expression
final case class Multiply(left: Expression, right: Expression) extends Expression

//  2 * 3 + 4
val expression = Add(
  Multiply(Const(2), Const(3)),
  Const(4)
)

def evaluate(expression: Expression): Int = expression match {
  case Const(n)              => n
  case Add(left, right)      => evaluate(left) + evaluate(right)
  case Multiply(left, right) => evaluate(left) * evaluate(right)
}

def show(expression: Expression): String = expression match {
  case Const(n)              => s"$n"
  case Add(left, right)      => s"${evaluate(left)} + ${evaluate(right)}"
  case Multiply(left, right) => s"${evaluate(left)} * ${evaluate(right)}"
}


trait ExpressionA[A] {
  def const(x: Int): A
  def add(left: A, right: A): A
  def multiply(left: A, right: A): A
}

val intAlgebra = new ExpressionA[Int] {
  override def const(x: Int) = x
  override def add(left: Int, right: Int) = left + right
  override def multiply(left: Int, right: Int) = left * right
}

val stringAlgebra = new ExpressionA[String]{
  override def const(x: Int) = x.toString
  override def add(left: String, right: String) = s"($left + $right)"
  override def multiply(left: String, right: String) = s"$left * $right"
}
// (10 + 5) / 5

def program2[A](algebra: ExpressionA[A]): A = {
  import algebra._

  add(
    multiply(const(2), const(3)),
    const(4)
  )
}

program2(intAlgebra)

trait ExtExpressionA[A] {
  def divide(left: A, right: A): A
}


def program3[A](algebra: ExpressionA[A], ext: ExtExpressionA[A]): A = {
  import algebra._, ext._

  multiply(
    divide(const(6), const(3)),
    const(2)
  )

}




trait ExpressionTF[F[_], A] {
  def const(x: A): F[A]
  def add(left: F[A], right: F[A]): F[A]
  def multiply(left: F[A], right: F[A]): F[A]
  def divide(left: F[A], right: F[A]): F[A]
}

object ExpressionTF {
  def intAlg[F[_]: Monad: MonoidK]: ExpressionTF[F, Int] = new ExpressionTF[F, Int] {
    override def const(x: Int) = x.pure
    override def add(left: F[Int], right: F[Int]) =
      (left, right).mapN(_ + _)
    override def multiply(left: F[Int], right: F[Int]) =
      (left, right).mapN(_ * _)
    override def divide(left: F[Int], right: F[Int]) =
      (left, right).tupled.flatMap{
        case (_, y) if y == 0 => MonoidK[F].empty
        case (x, y) => Monad[F].pure(x / y)
      }
  }
}