/*
Bronson Schultz, 11231230, bcs269
CMPT 340 A3 Q1
 */
case object problem1 {

  val royalParent = Map("August" -> ("m", "Eugenie", "Jack"), "George" -> ("m", "William", "Catherine"),
  "Charlotte" -> ("f", "William", "Catherine"), "Louis" -> ("m", "William", "Catherine"),
  "Archie" -> ("m", "Harry", "Meghan"), "Savannah" -> ("f", "Autumn", "Peter"),
  "Isla" -> ("f", "Autumn", "Peter"), "Mia" -> ("f", "Zara", "Mike"), "Lena" -> ("f", "Zara", "Mike"),
  "Beatrice" -> ("f", "Andrew", "Sarah"), "Eugenie" -> ("f", "Andrew", "Sarah"), "Louise" -> ("f", "Edward", "Sophie"),
  "James" -> ("m", "Edward", "Sophie"), "Peter" -> ("m", "Mark", "Anne"), "Zara" -> ("f", "Mark", "Anne"),
  "William" -> ("m", "Diana", "Charles"), "Harry" -> ("m", "Diana", "Charles"),
  "Charles" -> ("m", "Elizabeth", "Philip"), "Anne" -> ("f", "Elizabeth", "Philip"),
  "Andrew" -> ("m", "Elizabeth", "Philip"), "Edward" -> ("m", "Elizabeth", "Philip"), "Elizabeth" -> ("f", "", ""),
  "Philip" -> ("m", "", ""), "Diana" -> ("f", "", ""), "Mark" -> ("m", "", ""), "Sophie" -> ("f", "", ""),
  "Sarah" -> ("f", "", ""), "Mike" -> ("m", "", ""), "Autumn" -> ("f", "", ""), "Meghan" -> ("f", "", ""),
  "Catherine" -> ("f", "", ""), "Timothy" -> ("m", "", ""), "Jack" -> ("m", "", ""), "Camilla" -> ("f", "", ""))

  def parents(p: String): Either[String, (String, String)] = {
    royalParent.get(p) match {
      case Some((_,"","")) => Left(p + " has no parental info")
      case Some((_,m,d)) => Right((m,d))
      case None => Left(p + " is not in the royal family")
    }
  }

  def grandparents(p: String): Either[String, List[String]] = {
    val parent = parents(p)
    parent match {
      case Right((m: String,d: String)) =>
        val mps = parents(m).getOrElse(("",""))
        val dps = parents(d).getOrElse(("",""))

        Right(List(mps._1, mps._2, dps._1, dps._2).filterNot(_ == ""))
      case Left(error) => Left(error)
    }
  }

  def siblings(p: String): Either[String, List[String]] = {
    val parent = parents(p)
    lazy val sibs = LazyList()
    parent match {
      case Right((m: String, d: String)) =>
        for (person <- royalParent.keys) {
          val a = parents(person).getOrElse(("",""))
          if (a == (m,d) || a == (d,m)) {
            sibs.prepended(person)
          }

        }
        Right(sibs.toList)
      case Left(error) => Left(error)
    }
  }
  def main(args: Array[String]): Unit = {
    println("parents of August should be (Eugenie,Jack)")
    println(parents("August"))
    println("--------------------")

    println("parents of 123 should be a Left")
    println(parents("123"))
    println("--------------------")

    println("parents of Philip should be a Left")
    println(parents("Philip"))
    println("--------------------")

    println()

    println("August's grandparents should be Andrew and Sarah")
    println(grandparents("August"))
    println("--------------------")

    println("Elizabeth's grandparents don't exist, pass through the error from parents()")
    println(grandparents("Elizabeth"))
    println("--------------------")

    println(siblings("George"))

  }
}
