/**
  * Created by vpatryshev on 7/21/16.
  */
private [parsing] object ExtractorOps {

  val AllDigits = "(\\d+)".r

  def intOf(node: Node, attr: String, altValue: Int) = {
    node attribute attr map (_.text
    match {
      case AllDigits(dd) ⇒ dd.toInt
      case stuff ⇒ altValue
    }) getOrElse altValue
  }

  def cellWidth(node: Node)  :Int    = intOf(node, "colspan", 1)
  def cellHeight(node: Node) :Int    = intOf(node, "rowspan", 1)

  def trimKey(key: String) = {
    val found = key split "\n"  find(!_.isEmpty) getOrElse ""
    val trimmed = found split "\\." map (_.trim) mkString "."
    trimmed
  }

  def cleanupText(text:String) = {
    val result = text.replaceAll("$", "").replaceAll("[\uFFF9-\uFFFE]", "").replaceAll("\u00a0","").replaceAll("\n", " ").replaceAll(" +", " ").trim
    val Equalities = "=?(.*)=?".r
    val cleanText = result match {
      case Equalities(content) ⇒ content
      case other ⇒ other
    }
    if (cleanText == "-") "" else cleanText
  }

  def textOf(node: Node): String = {
    val mergeWith = node.child map textOf mkString (_:String)

    val txt = node.label match {
      case "td"|"th"|"table"|"tbody"|"tr" ⇒ mergeWith(" ")
      case "div"|"span" ⇒ mergeWith("")
      case other ⇒ node.text
    }
    cleanupText(txt)
  }
}
