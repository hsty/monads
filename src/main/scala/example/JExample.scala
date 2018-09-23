package example


object JExample extends App{

  trait JSon
  case class JSeq( elems: List[JSon]) extends JSon
  case class JObj(bindings: Map[String, JSon]) extends JSon
  case class JNum (num: Double) extends JSon
  case class JStr (str: String) extends JSon
  case class JBool(b: Boolean) extends JSon
  case object JNull extends JSon

  val data = JObj(Map(
    "firstname" -> JStr("John"),
    "lastname" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21st 2nd avenue"),
      "state" -> JStr("NY"),
      "postalcode" -> JNum(10021)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"), "number" -> JStr("4444444444")
      )),
      JObj(Map(
        "type" -> JStr("work"), "number" -> JStr("8567922587")
      ))
    ))
  ))

  def show(json: JSon): String = json match {
    case JSeq(elems) =>
      "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\":" + show(value)
      }
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b) => b.toString
    case JNull => "null"
  }

  println(show(data))


  val f: PartialFunction[String, String] = { case "ping" => "pong"}

  println(f.isDefinedAt("abc"))
/*
  val k = for {
    JObj(bindings) <- data
    JSeq(phones) = bindings("phoneNumbers")
    JObj(phone) <- phones
    JStr(digits) = phone("number")
    if digits.startsWith("212")
  } yield(bindings("firstname"), bindings("lastname"))


  println(k)
*/
  case class Book(title: String, authors: List[String])


  val hObj = new hRandom()
  //println(hObj.trees.generate)


  println(hObj.test(hObj.pairs(hObj.lists,hObj.lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  })
}
