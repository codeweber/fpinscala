package fpinscala.answers.localeffects

//STRef is a sealed trait; hence it can only be instantiated within this source file.
sealed trait STRef[S,A]:
    protected var cell: A 
    def read: ST[S,A] = ST(cell)
    def write(a: A): ST[S,Unit] = 
        new ST[S,Unit]:
            def run(s: S) =
                cell = a
                ((), s) 


object STRef:
    //The only way to create an instance of STRef is through the apply factory method
    //This does not create STRef directly, but creates it within the context of ST.
    def apply[S,A](a: A): ST[S, STRef[S,A]] =
        ST(new STRef[S,A] {var cell = a})