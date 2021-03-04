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

  def parent(p: String): Either[String, (String, String)] = {
    royalParent.get(p) match {
      case Some((_,"","")) => Left(p + " has no parental info")
      case Some((_,m,d)) => Right((m,d))
      case None => Left(p + " is not in the royal family")
    }
  }

  def grandparents(p: String): Either[String, List[String]] = {
    val parents = parent(p)
    parents match {
      case Right((m,d)) =>
        val mps = parent(m)
        val dps = parent(d)
        println("mom's parents " + mps.getOrElse(Left(p+"'s mom " + m + "has no parental info")))
        println("Dad's parents " + dps.getOrElse(Left(p+"'s dad "+ d + "has no parental info")))
        Right(List())
    }
  }

  def main(args: Array[String]): Unit = {
    println("parents of August should be (Eugenie,Jack")
    println(parent("August"))
    println("parents of 123 should be a Left")
    println(parent("123"))
    println("parents of Philip should be a Left")
    println(parent("Philip"))

    println(grandparents("August"))
  }
}
