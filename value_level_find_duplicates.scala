package ncreep.error_generation

// a value-level equivalent of the code in `find_duplicates.scala`

@main def testFindDuplicates(): Unit =
  val labellings1 =
    List(
      ValueLabelling("Email2", List("primary", "secondary")),
      ValueLabelling("Phone2", List("number", "prefix")),
      ValueLabelling("Address2", List("country", "city"))
    )

  val labellings2 =
    List(
      ValueLabelling("Email2", List("primary", "secondary", "lastUpdate", "verified")),
      ValueLabelling("Phone2", List("number", "prefix", "verified")),
      ValueLabelling("Address2", List("country", "city", "lastUpdate"))
    )

  println(findDuplicates(labellings1))
  println(findDuplicates(labellings2))
end testFindDuplicates

case class ValueLabelling(label: String, elemLabels: List[String])

def findDuplicates(labellings: List[ValueLabelling]): List[(String, List[String])] =
  onlyDuplicates(groupByLabels(zipAllWithSource(labellings)))

def zipAllWithSource(labellings: List[ValueLabelling]): List[(String, String)] =
  labellings.flatMap(zipWithSource)

def groupByLabels(labels: List[(String, String)]): List[(String, List[String])] =
  labels match // a non-efficient groupBy implementation
    case Nil => Nil
    case (label, source) :: tail =>
      (label, source :: findLabel(label, tail)) :: groupByLabels(removeLabel(label, tail))

def onlyDuplicates(labels: List[(String, List[String])]): List[(String, List[String])] =
  labels.filter(_._2.size > 1)

def findLabel(label: String, labels: List[(String, String)]): List[String] =
  labels.filter(ls => hasLabel(label, ls)).map(_._2)

def removeLabel(label: String, labels: List[(String, String)]): List[(String, String)] =
  labels.filter(ls => !hasLabel(label, ls))

def hasLabel(label: String, labelSource: (String, String)): Boolean =
  label == labelSource._1

def zipWithSource(labelling: ValueLabelling): List[(String, String)] =
  zipWithConst(labelling.elemLabels, labelling.label)

def zipWithConst[A, B](ls: List[A], const: B): List[(A, B)] =
  ls.map((_, const))
