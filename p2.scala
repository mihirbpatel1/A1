import scala.util.Success
class p2 {
  sealed trait Partial[+A,+B]{

    case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
    case class Success[+B](get: B) extends Partial[Nothing, B]

    def map[C](f: B => C) : Partial[A,C] = this match{

      case Success(a) => Success(f(a))
      case Errors(a) => Errors(a)

    }

    def flatMap[AA >: A,C](f: B => Partial[AA,C]):Partial[AA,C]= this match{

      case Success(a) => f(a)
      case Errors(a) => Errors(a)

    }

    def getOrElse[AA >: A,C >: B](f: => C):C= this match{

      case Success(a) => a
      case Errors(_) => f

    }





  }


}
