package fpinscala.answers.localeffects

trait ST[S,A]:
    self =>

    //Note that run is a protected method; it can only be access by instances or subtypes
    //This is important, because the type S implies that this type CAN mutate state and
    //these mutations cannot be permitted to escape.
    protected def run(s: S): (A,S)

    def map[B](f: A => B): ST[S,B] =
        new ST[S,B]:
            def run(s: S): (B,S) = 
                val (a,s1) = self.run(s)
                (f(a), s1)

    def flatMap[B](f: A => ST[S,B]): ST[S,B] =
        new ST[S,B]:
            def run(s: S): (B,S) =
                val (a,s1) = self.run(s)
                f(a).run(s1)


trait RunnableST[A]:
    def apply[S]: ST[S,A]

object ST:

    def apply[S,A](a: => A) =
        lazy val memo = a 
        new ST[S,A]:
            def run(s: S): (A,S) = (memo, s)

    def runST[A](st: RunnableST[A]): A =
        st.apply[Unit].run(())._1