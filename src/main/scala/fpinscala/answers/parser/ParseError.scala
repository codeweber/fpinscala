package fpinscala.answers.parser

case class ParseError(stack: List[(Location, String)]):

    def push(loc: Location, msg: String): ParseError =
        copy(stack = (loc,msg) :: stack)

    def label(msg: String): ParseError =
        ParseError(latestLoc.map((_, msg)).toList)

    def latestLoc: Option[Location] =
        latest.map(_._1)

    def latest: Option[(Location, String)] =
        stack.lastOption