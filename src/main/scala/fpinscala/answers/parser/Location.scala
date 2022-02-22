package fpinscala.answers.parser

case class Location(input: String, offset: Int = 0):

    lazy val line = input.slice(0, offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset+1).lastIndexOf('n') match
        case -1 => offset+1
        case lineStart => offset-lineStart


    def toError(msg: String): ParseError =
        ParseError(List((this, msg)))

    def advanceBy(n: Int): Location =
        copy(offset = offset + n)

    def remaining: String =
        input.drop(offset)

    def slice(n: Int) =
        input.slice(offset, offset+n)

end Location