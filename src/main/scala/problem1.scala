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
    var sibs = List[String]()
    parent match {
      case Right((m: String, d: String)) =>
        for (person <- royalParent.keys) {
          if (!person.equals(p)) {
            if (parents(person).getOrElse(("", "")) == (m, d) || parents(person).getOrElse(("", "")) == (d, m)) {
              sibs = person :: sibs
            }
          }
        }
        Right(sibs)
      case Left(error) => Left(error)
    }
  }

  def firstCousins(p: String): Either[String, List[String]] = {
    var cousins = List[String]()
    for (person <- royalParent.keys) {
      if (!person.equals(p) && !siblings(p).getOrElse(("","")).toString.contains(person)) {
        if (grandparents(person) == grandparents(p)) {
          cousins = person :: cousins
        }
      }
    }
    Right(cousins)
  }

  def uncles(p: String): Either[String, List[String]] = {
    val parent = parents(p)
    if (parent.isRight) {
      val royalAsAndUs: List[String] = siblings(parent.getOrElse(("",""))._1).getOrElse(Nil)
      Right(royalAsAndUs.filter(royalParent(_)._1 == "m"))
    } else {
      Left(p + " has no parental information to find their royal uncle(s)")
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

    println("George's siblings should be Charlotte and Louis")
    println(siblings("George"))

    println("George's first cousins should be Archie")
    println(firstCousins("George"))

    println("Elizabeth's siblings should be NiL")
    println(siblings("Elizabeth"))


    println(uncles("George"))

  }
}
