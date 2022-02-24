package fpinscala.answers.parser

enum JSON:
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])
  case JNull 


object JSON:

  def jsonParser[Parser[+?]](P: Parsers[Parser]): Parser[JSON] = ???





