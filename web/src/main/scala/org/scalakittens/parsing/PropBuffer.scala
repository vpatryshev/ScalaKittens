package org.scalakittens.web.common.parsing

import org.scalakittens.Library._
import org.scalakittens.web.common.parsing.PropBuffer._
import org.scalakittens.Props

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

/**
 * Holds properties while they are being parsed - extracted from sources
 * Created by vpatryshev on 2/5/14.
 */
class PropBuffer(val label:String, val properties: Props = Props.empty, val rawPrefixes: List[String] = Nil) {

  def withLabel(newLabel: String) = {
    if (newLabel == "div") this // forget divs eh
    else new PropBuffer(newLabel, properties, rawPrefixes)
  }

  def filterPrefixes(prefixes:List[String]):List[String] = prefixes map (_.replaceAll("\\.,", "").trim) filter (!_.isEmpty)
  def addNumber(i:Int) = new PropBuffer(label, allProperties.addNumber(i), prefixes)

  def isEmpty = properties.isEmpty && prefixes.isEmpty
  val prefixes = filterPrefixes(rawPrefixes)
  def hasData = !properties.isEmpty
  val propertiesAndPrefixes = prefixes.partition(_ matches regexForKeyAndValue)
  val newKVpairs:Seq[(String, String)] = propertiesAndPrefixes._1 map {case KeyAndValue(k, v) => k→v}
  val extraProps = props(newKVpairs toMap)
  val singlePrefixes = propertiesAndPrefixes._2
  val (newProps, extraPrefixes) = extractPropertiesFromPrefixes(singlePrefixes)
  val allProperties = properties ++ extraProps ++ newProps
  def ++(morePrefixes: List[String]) =
    morePrefixes.isEmpty ? this | new PropBuffer(label, allProperties, extraPrefixes ++ morePrefixes)

  def merge(other: PropBuffer): PropBuffer = {
    val itIsAllText = this.label == "#PCDATA" && other.label == "#PCDATA" // TODO(vlad): it's obvious that we have to subclass
    val newLabel = if (itIsAllText) label else label::other.label::Nil filter (!_.isEmpty) mkString ","
    val additionalProperties: Props = other.allProperties.addPrefixes(extraPrefixes)
    val newProps = allProperties ++ additionalProperties
    val newPrefixes = extraPrefixes ++ other.extraPrefixes

    val merged = if (other.isEmpty) this
    else if (this.isEmpty) other
    else if (other.allProperties.isEmpty) {
      val mergedPrefixes = if (itIsAllText && newPrefixes.length == 2) newPrefixes.mkString(" ") :: Nil else newPrefixes
      val np = if (mergedPrefixes.length == 2 && mergedPrefixes(0).endsWith(":") && !mergedPrefixes(1).endsWith(":") && mergedPrefixes(1).nonEmpty) {
        val key: String = mergedPrefixes(0).dropRight(1).replaceAll(":", ".").trim
        val newProp = props(key → mergedPrefixes(1).trim)
        new PropBuffer(newLabel, newProps ++ newProp, Nil)
      } else {
        new PropBuffer(newLabel, newProps, mergedPrefixes)
      }

      val newAllProps = np.allProperties
      val newShortPrefixes = np.extraPrefixes
      new PropBuffer(newLabel, newAllProps, newShortPrefixes) // TODO(vlad): fix this, it's stupid
    } else {
      val thisLabel = label.split(",").head
      val usePrefixes =  if (beats(thisLabel, other.label)) newPrefixes else Nil
      new PropBuffer(newLabel, newProps, usePrefixes)
    }

    merged
  }

  def beats(label1:String, label2:String) = Set(("h2", "ul")) contains (label1,label2)

  def processRemainingPrefixes = {
    val (newProps, newPrefixes) = extractPropertiesFromPrefixes(prefixes)
    new PropBuffer(label, allProperties ++ newProps, (newPrefixes mkString " ") :: Nil)
  }

  def convertToProperties = {
    val allLabels = label split "," toSet
    def isHomogeneous = allLabels.size == 1

    if (properties.isEmpty && prefixes.nonEmpty && prefixes.length %2 == 0 && isHomogeneous) {
      def kvPairs = prefixes.grouped(2)
      val keyMap = kvPairs .map (kv => kv(0).replaceAll(":","")→kv(1)) .toMap
      val keysOk = keyMap.keys.forall(_.toLowerCase.matches("[a-z].*"))

      if (keysOk) {
        val uniqueLabel = allLabels.headOption filter canConvertToProperties

        val converted = uniqueLabel map (
          new PropBuffer(_, props(keyMap), Nil)
          ) getOrElse this
        converted
      } else this
    } else this
  }

  def dropPrefixes = if (prefixes.isEmpty) this else new PropBuffer(label, properties)

  def prependPrefix(prefix: String) = PropBuffer.justPrefixes(label, prefix::Nil).merge(this)

  override def toString = s"PropBuffer('$label, $properties, $rawPrefixes)"
  val PrefixTypesThatCannotBeConvertedToProperties = "img,li,a" split "," toSet
  def canConvertToProperties(label: String) = !PrefixTypesThatCannotBeConvertedToProperties(label)
}

class HeaderPropBuffer(props: Props, prefixes: List[String], numRows: Int = 0)
  extends PropBuffer("HeaderTable", props, prefixes) {
  def this(source: PropBuffer) = this(source.properties, source.prefixes)

  import Result._

  override def merge(other: PropBuffer): PropBuffer = {
    if (other.isEmpty) this else {
      val haveContinuation = OKif(other.label == "table", s"no more data tables after $numRows rows") andAlso
        OKif(other.prefixes.size == prefixes.size, s"different width: have ${prefixes.size}, got ${other.prefixes.size}")
      if (!haveContinuation) {
        super.merge(other)
      } else {
        val newLabel = label
        val newNumRows = numRows + 1
        val additionalProperties: Props = Props((prefixes map (s"[[$newNumRows]]."+) zip other.prefixes).toMap)
        val newProps = allProperties ++ additionalProperties
        val merged = new HeaderPropBuffer(newProps, prefixes, newNumRows)

        merged
      }
    }
  }
}

object PropBuffer {
  val regexForKeyAndValue = "\\s*([^:]+)\\s*:\\s*([^:]+)\\s*"
  val KeyAndValue = regexForKeyAndValue.r

  private[scalakittens] def justPrefixes(label:String, prefixes:List[String] = Nil) = new PropBuffer(label, Props.empty, prefixes map transformer)

  private[scalakittens] def empty = justPrefixes("")

  def extractPropertiesFromPrefixes(prefixes:List[String]):(Props, List[String]) = {
    if (prefixes.length > 0 && prefixes.length %2 == 0) {
      val joined = prefixes map (_.replaceAll("\\.", "~~")) mkString "."
      val pairsDetected = joined replaceAll(":\\.", ":")
      val split = pairsDetected.split("\\.")

      def split2pair(s:String):(String, Option[(String,String)]) = s → (s.split(":",3).toList match {
          case k::v::Nil => Some(k,v)
          case _         => None
        })

      val tuples: Array[(String, Option[(String, String)])] = split map split2pair
      val (newProps, newPrefixes) = tuples partition (_._2.isDefined)
      val mapOfExtraProperties:Map[String, String] = newProps.map(_._2.get) toMap
      val extraProperties: Props = props(mapOfExtraProperties.mapValues(_.replaceAll("~~", ".")))
      val result = (extraProperties, newPrefixes map (_._1.replaceAll("~~", ".")) toList)
      result
    } else (props(), prefixes)
  }

  trait TableBufferBuilder {
    def apply(): PropBuffer
  }

  class RegularTableBufferBuilder(cells: List[Row]) extends TableBufferBuilder {

    private def groupTableRows(rows: Seq[Row]): Int Map Seq[Row] = {
      val labels: List[Int] = rows.foldLeft[(List[Int], Int)]((Nil, 1))(
        (e:(List[Int], Int), row: Row) => {
          row.hasData ? (0::e._1, e._2+1) | (e._2::e._1, e._2)
        }
      )._1.reverse

      val m = rows zip labels groupBy (_._2)
      val res = m mapValues (s => s.map(_._1)) withDefaultValue Nil
      res
    }

    val plainKeyValuePairs: PropBuffer = extractKeyValuePairs(cells)
    lazy val (leadingRows: List[Row], rowsWithHeader: List[Row]) = cells.span(_.justValues)
    val rowsWeNeed: List[Row] = rowsWithHeader.isEmpty ? cells | rowsWithHeader
    val rowGroups: Map[Int, Seq[Row]] = groupTableRows(rowsWeNeed)
    val group0: Seq[Row] = rowGroups(0)
    val cellsOfGroup0: Seq[List[Cell]] = group0 map (_.cells)
    val cellsByRow: Seq[List[Cell]] = cellsOfGroup0 filter (_.nonEmpty)
    val cellsWithPrefix: Seq[List[Cell]] = cellsByRow.zipWithIndex map { case (row, rowNo) => row map (_ withIndex (rowNo + 1)) }
    val cellsWithProps: Seq[Cell] = cellsWithPrefix.flatten
    val nestedProps: Seq[PropBuffer] = cellsWithProps map (_.data)
    val localRowGroups: Iterable[Seq[Row]] = rowGroups.filterKeys(0 <).values
    val localProps: Iterable[PropBuffer] = localRowGroups map extractPropsFromTableRows
    val res: PropBuffer = (justPrefixes("table") /: (nestedProps ++ localProps)) (_ merge _)
    val haveLocalData:Boolean = localRowGroups.nonEmpty && localRowGroups.head.nonEmpty
    val structuredTableContents: PropBuffer = res.properties.nonEmpty ? res |
      (haveLocalData ? {
        new PropBuffer("table", Props.empty, localRowGroups.head.head.textSegments)
      } | PropBuffer.empty
        )
    def apply(): PropBuffer = structuredTableContents merge plainKeyValuePairs
  }

  class HeaderTableBufferBuilder(cells: List[Row]) extends TableBufferBuilder {
    private val row: Row = cells(0)

    val prefixes = row.cells.map(_.fullText)
    val buf = new HeaderPropBuffer(Props.empty, prefixes)

    override def apply(): PropBuffer = buf
  }
}
