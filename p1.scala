object p1 extends App {
  val royalParent = Map("George" -> ("m", "William", "Catherine"), "Charlotte" -> ("f", "William", "Catherine"),
    "Louis" -> ("m", "William", "Catherine"), "Archie" -> ("m", "Harry", "Meghan"),  "Lilibet" -> ("f", "Harry", "Meghan"),
    "Savannah" -> ("f", "Autumn", "Peter"), "Isla" -> ("f", "Autumn", "Peter"), "Mia" -> ("f", "Zara", "Mike"),
    "Lena" -> ("f", "Zara", "Mike"), "Lucas" -> ("m", "Zara", "Mike"), "Sienna" -> ("f", "Beatrice", "Edoardo"),
    "August" -> ("m", "Eugenie", "Jack"), "Beatrice" -> ("f", "Andrew", "Sarah"), "Eugenie" -> ("f", "Andrew", "Sarah"),
    "Louise" -> ("f", "Edward", "Sophie"), "James" -> ("m", "Edward", "Sophie"), "Peter" -> ("m", "Mark", "Anne"),
    "Zara" -> ("f", "Mark", "Anne"), "William" -> ("m", "Diana", "Charles"), "Harry" -> ("m", "Diana", "Charles"),
    "Charles" -> ("m", "Elizabeth", "Philip"), "Anne" -> ("f", "Elizabeth", "Philip"),
    "Andrew" -> ("m", "Elizabeth", "Philip"), "Edward" -> ("m", "Elizabeth", "Philip"),
    "Elizabeth" -> ("f", "", ""), "Philip" -> ("m", "", ""), "Diana" -> ("f", "", ""),
    "Mark" -> ("m", "", ""), "Sophie" -> ("f", "", ""), "Sarah" -> ("f", "", ""),
    "Mike" -> ("m", "", ""), "Autumn" -> ("f", "", ""), "Meghan" -> ("f", "", ""),
    "Catherine" -> ("f", "", ""), "Timothy" -> ("m", "", ""), "Jack" -> ("m", "", ""),
    "Camilla" -> ("f", "", ""), "Edoardo" -> ("m", "", ""))


  def parents(p: String): Option[(String, String)]= royalParent.get(p) match{
    case None => None
    case Some((_,"","")) => Some("Parent:", "not found")
    case Some((_,a,b))=> Some(a,b)
  }

  def grandparents(p: String): Option[List[String]] = parents(p) match{
    case None => None
    case Some((dad,mom))=> parents(dad)
      .map(list =>List(list._1, list._2))
      .orElse(parents(mom)
        .map(list =>List(list._1, list._2)))

  }
  def brothers(p: String): Option[List[String]]= parents(p) match{
    case None => None
    case Some(x)=>Some(royalParent.view.filterKeys(c=>parents(p)== parents(c)
      && p !=c && royalParent(c)._1=="m").keys.toList)
  }

  def firstCousins(p: String): Option[List[String]] = grandparents(p) match{
    case None=> None
    case Some(x)=> Some(royalParent.view.filterKeys(c=> grandparents(p)==grandparents(c)
      && p!= c && parents(p) != parents(c)).keys.toList)
  }
  def aunts(p: String): Option[List[String]] = parents(p) match{// only aunts who are sisters of a parent, not parent's brothers' spouses

    case None=> None
    case Some(x)=>Some(royalParent.view.filterKeys(c=>parents(p)== parents(c)
      && p !=c && royalParent(c)._1=="f").keys.toList)
  }

  println("Harry Family:\n" + parents("Harry")  + grandparents("Harry")  + brothers("Harry") + firstCousins("Harry")+ aunts("Harry"))



}
