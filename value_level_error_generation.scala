// a value-level equivalent of the code in `error_generation.scala`

@main def testErrorGeneration(): Unit =
  val labelsWithSources = List(
    ("lastUpdate", List("Email2", "Address2")), 
    ("verified", List("Email2", "Phone2")))

  println(renderError(labelsWithSources))
end testErrorGeneration

def renderError(labelsWithSources: List[(String, List[String])]): String = 
  renderGroups(labelsWithSources).fold("")(_ ++ _)

def renderGroups(labelsWithSources: List[(String, List[String])]) = 
  labelsWithSources.map(renderGroup)

def renderGroup(labelWithSources: (String, List[String])): String = 
  val (label, sources) = labelWithSources

  "- [" ++ label ++ "] from [" ++ sources.mkString(", ") ++ "]\n"

