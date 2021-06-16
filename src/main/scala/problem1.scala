/*
Bronson Schultz, 11231230, bcs269
CMPT 340 A3 Q1
 */

// object to hold the family tree and it's relevant functions
case object problem1 {

  // map of the royal family tree. Keys are members' names and values are tuples containing their parents' names and gender
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

  /**
  Given a name return the person's parents from the map
  
  param p: the name of person who's parents you're looking for
  return: Either a right containing the person's parent's names or a left containing an error message
  **/
  def parents(p: String): Either[String, (String, String)] = {
    royalParent.get(p) match {
      case Some((_,"","")) => Left(p + " has no parental info")
      case Some((_,m,d)) => Right((m,d))
      case None => Left(p + " is not in the royal family")
    }
  }
  
  /**
  given a name, find that person's grandparents
  
  param p: the name of the person who's grandparents you're looking for
  return: Either a right containing a list of p's grandparents or a left with an error
  message if p's parental info is not in the map
  **/
  def grandparents(p: String): Either[String, List[String]] = {
    val parent = parents(p)
    parent match {
      case Right((m: String,d: String)) =>
        val mps = parents(m).getOrElse(("","")) // get p's mom's parents
        val dps = parents(d).getOrElse(("","")) // get p's dad's parents

        Right(List(mps._1, mps._2, dps._1, dps._2).filterNot(_ == "")) // put all grandparents in a list and filter out any empty strings
      case Left(error) => Left(error)
    }
  }

  /**
  given a name, find that person's siblings
  
  param p: the name of the person who's siblings you're looking for
  return: Either a right containing a list of p's siblings or a Left if an error occurred along the way
  **/
  def siblings(p: String): Either[String, List[String]] = {
    val parent = parents(p)
    var sibs = List[String]()
    parent match {
      case Right((m: String, d: String)) =>
        for (person <- royalParent.keys) { // for each persn in the map
          if (!person.equals(p)) { // if person is not p
            if (parents(person).getOrElse(("", "")) == (m, d) || parents(person).getOrElse(("", "")) == (d, m)) { // and p and person share parents
              sibs = person :: sibs // then they must be siblings
            }
          }
        }
        Right(sibs)
      case Left(error) => Left(error)
    }
  }

  /**
  given a person's name, find their first cousins
  
  param p: the name of the person
  return: a right containing a list of all of p's first cousins
  **/
  def firstCousins(p: String): Either[String, List[String]] = {
    var cousins = List[String]()
    for (person <- royalParent.keys) { // for each person in the map
      if (!person.equals(p) && !siblings(p).getOrElse(("","")).toString.contains(person)) { // if the person is not p or siblings of p
        if (grandparents(person) == grandparents(p)) { // if person shares grandparents with p
          cousins = person :: cousins // then p and person must be first cousins
        }
      }
    }
    Right(cousins)
  }

  /**
  Given a name, find all of their royal uncles
  param p: name of the person
  return: a right containing a list of p's royal uncles or a left containing a error message
  **/
  def uncles(p: String): Either[String, List[String]] = {
    val parent = parents(p)
    if (parent.isRight) { // if p has parental info
      val royalAsAndUs: List[String] = siblings(parent.getOrElse(("",""))._1).getOrElse(Nil) // find p's royal parent's siblings
      Right(royalAsAndUs.filter(royalParent(_)._1 == "m")) // filter out the aunts and return
    } else {
      Left(p + " has no parental information to find their royal uncle(s)")
    }
  }

  def main(args: Array[String]): Unit = {
    
    // parents tests
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

    // grandparents tests
    println("August's grandparents should be Andrew and Sarah")
    println(grandparents("August"))
    println("--------------------")

    println("Elizabeth's grandparents don't exist, pass through the error from parents()")
    println(grandparents("Elizabeth"))
    println("--------------------")

    // siblings tests
    println("George's siblings should be Charlotte and Louis")
    println(siblings("George"))
    
    println("Elizabeth's siblings should be NiL")
    println(siblings("Elizabeth"))
    
    println("Elizabeth's siblings should be NiL")
    println(siblings("Elizabeth"))
    
    // first cousins test

    println("George's first cousins should be Archie")
    println(firstCousins("George"))

    println(firstCousins("Archie"))
    println("Archie's first cousins should be Louis, Charlotte, George")
    
    // uncles test
    println("George's uncle should be Harry")
    println(uncles("George"))
    
    println("Savannah's uncles should be a NIL list")
    println(uncles("Savannah"))
    
    println("James's royal uncles should be Andrew, Charles")
    println(uncles("James"))
  }
}
