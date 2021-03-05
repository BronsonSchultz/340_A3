sealed trait Partial[+A,+B]{
  def map[C](f: B => C): Partial[A,C] = {
    this match {
      case Success(b) => Success(f(b))
      case Errors(es) => Errors(es)
    }
  }

  def flatMap[AA >: A, C](f: B => Partial[AA,C]): Partial[AA,C] = {
    this match {
      case Success(b) => f(b)
      case Errors(es) => Errors(es)
    }
  }

  def orElse[AA >: A, C >: B](z: => Partial[AA,C]): Partial[AA,C] = {
    this match {
      case Errors(_) => z
      case Success(b) => Success(b)
    }
  }

  def map2[AA >: A, C, D](b: Partial[AA,C])(f: (B,C) => D):
    Partial[AA,D] = for {
      a <- this
      b1 <- b
    } yield f(a,b1)
}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

object Partial {
  def traverse[A,B,C](as: List[B])(f: B => Partial[A,C]): Partial[A,List[C]] = {
    as match {
      case Nil => Success(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
  }

  def Try[A](a: => A): Partial[Exception, A] =
    try Success(a)
    catch { case e: Exception => Errors(Seq(e))}


  def main(args: Array[String]): Unit = {
    var p = Success(1).map(_ + 1)
    println(p)

    var e = Errors(Seq("e")).map(println)
    println(e)
    
  }
}



