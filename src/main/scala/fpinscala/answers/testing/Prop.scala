package fpinscala.answers.testing

import fpinscala.answers.state.RNG

type TestCases = Int 
type FailedCase = String
type SuccessCount = Int 
type PropLabel = String
type MaxSize = Int

enum Result:
        case Passed 
        case Falsified(failure: FailedCase, successess: SuccessCount)

        def isFalsified: Boolean =
            this match
                case Passed => false
                case Falsified(_, _) => true

end Result

case class Prop(run: (MaxSize, TestCases, RNG) => Result):

    import Result.*

    def &&(that: Prop): Prop =
        Prop {
            (maxSize, n, rng) =>
                val resultThis = this.run(maxSize, n, rng)
                resultThis match
                    case Passed => 
                        val resultsThat = that.run(maxSize, n, rng)
                        resultsThat match
                            case Passed => Passed
                            case Falsified(f, s) => Falsified("Right Prop failed\n" + f, s)
                    case Falsified(f, s) => Falsified("Left Prop failed\n" + f, s)
        }

    def ||(that: Prop): Prop =
        Prop {
            (maxSize, n, rng) =>
                val resultThis = this.run(maxSize, n, rng)
                resultThis match
                    case Passed => Passed                        
                    case Falsified(f, s) => 
                        val resultsThat = that.run(maxSize, n, rng)
                        resultsThat match
                            case Passed => Passed
                            case Falsified(f, s) => Falsified("Left and Right Props failed\n" + f, s)

        }

object Prop:

    import Gen.*
    import Result.*

    def buildMsg[A](s: A, e: Exception): String = 
        s"""|test case: $s
            |generated an exception: ${e.getMessage}
            |stack trace:
            |${e.getStackTrace.mkString("\n")}
            |""".stripMargin

    def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
        LazyList.unfold(rng)( r => Some(g.sample(r)) )


    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = 
        Prop {
            (max, n, rng) =>
                randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
                    case (a,i) =>
                        try
                            if f(a) then Passed else Falsified(a.toString, i)
                        catch
                            case e: Exception => Falsified(buildMsg(a,e), i)
                }.find(_.isFalsified).getOrElse(Passed)
        }


    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
        Prop {
            (maxSize, n, rng) =>

                val props = 
                    LazyList.from(0).take((n min maxSize) + 1).map( i => forAll(g(i))(f) )

                val casesPerSize = 1 + (n / maxSize)
                val prop = 
                    props.map( {
                        p =>
                            Prop {
                                (m,_,r) =>
                                    p.run(m, casesPerSize, r)
                            }
                    }).toList.reduce(_ && _)
                prop.run(maxSize, n, rng)

        } 

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
        forAll(g(_))(f)

end Prop