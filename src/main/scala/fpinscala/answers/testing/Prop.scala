package fpinscala.answers.testing

import fpinscala.answers.state.RNG
import javax.swing.text.PasswordView

type TestCases = Int 
type FailedCase = String
type SuccessCount = Int 
type PropLabel = String

enum Result:
        case Passed 
        case Falsified(failure: FailedCase, successess: SuccessCount)

        def isFalsified =
            this match
                case Passed => false
                case Falsified(_, _) => true

end Result

case class Prop(run: (TestCases, RNG) => Result, label: PropLabel):

    import Result.*

    def &&(that: Prop): Prop =
        Prop {
            (n, rng) =>
                val resultThis = this.run(n, rng)
                resultThis match
                    case Passed => 
                        val resultsThat = that.run(n, rng)
                        resultsThat match
                            case Passed => Passed
                            case Falsified(f, s) => Falsified("Right Prop failed\n" + f, s)
                    case Falsified(f, s) => Falsified("Left Prop failed\n" + f, s)
        }

    def ||(that: Prop): Prop =
        Prop {
            (n, rng) =>
                val resultThis = this.run(n, rng)
                resultThis match
                    case Passed => Passed                        
                    case Falsified(f, s) => 
                        val resultsThat = that.run(n, rng)
                        resultsThat match
                            case Passed => Passed
                            case Falsified(f, s) => Falsified("Left and Right Props failed\n" + f, s)

        }

object Prop:

    import Gen.*
    import Result.*

    def apply(r: (TestCases, RNG) => Result): Prop = apply(r, "")

    def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
        LazyList.unfold(rng)( r => Some(g.sample(r)) )


    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = 
        Prop {
            (n, rng) =>
                randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
                    case (a,i) =>
                        try
                            if f(a) then Passed else Falsified(a.toString, i)
                        catch
                            case e: Exception => Falsified(buildMsg(a,e), i)
                }.find(_.isFalsified).getOrElse(Passed)
        }

    def buildMsg[A](s: A, e: Exception): String = 
        s"""|test case: $s
            |generated an exception: ${e.getMessage}
            |stack trace:
            |${e.getStackTrace.mkString("\n")}
            |""".stripMargin
    

end Prop