package scalakittens

import Result._
import Props._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.json.JSON

/**
 * LDAP-like storage of properties with multilevel (dot-separated) keys
 */
case class Props(private val innerMap: PropMap) extends PartialFunction[String, String] { self =>
  def filterValues(predicate: String => Boolean) = props(innerMap.filter {case (k,v) => predicate(v)})

  implicit def ternary(b: =>Boolean) = new {def ?[T](x: =>T) = new { def |(y: =>T) = if(b) x else y}}

  import Props._

  private def keyAsArray(k: String):Array[String] = k split "\\."
  implicit def keyAsSet(k: String):Set[String] = keyAsArray(k) toSet

  def findKey(key: String): Option[String] = {
    def equalKeys = innerMap.keys.filter(key==)
    def matchingKeys = innerMap.keys.filter(keyMatches(key))
    def containingKeys = innerMap.keys.filter(keyContains(key))
    equalKeys.headOption orElse matchingKeys.headOption orElse containingKeys.headOption
  }
  def findAndReplace(key: String, value: String): Props = {
    val keysFound = findAllHaving(key)
    val delta = keysFound.innerMap mapValues (_ => value)
    this ++ props(delta)
  }

  def findKeyAnyOrder(key: String): Option[String] = innerMap.keys.find(sameKeyBundle(_, key))

  lazy val pfOpt = (key:String) => findKey(key) flatMap innerMap.get

  def apply(key: String) = innerMap(key)
  def find(collectionOfKeys: String*) = collectionOfKeys find isDefinedAt map apply
  def forSomeKey(collectionOfKeys: String*) = Result(find(collectionOfKeys:_*)) orCommentTheError s"Failed to find a prop for keys (${collectionOfKeys mkString ","})"
  def findIgnoringKeyOrder(keys: String):Option[String] = findKeyAnyOrder(keys) map apply
  def isDefinedAt(key: String): Boolean = innerMap.isDefinedAt(key)
  def get(key: String): Option[String] = pfOpt(key)
  def mapValues[U](f:String=>U) = innerMap andThen f
  def applyOrElse(key: String, otherwise: String => String): String = innerMap.applyOrElse(key, otherwise)
  def getOrElse(key: String, otherwise: String) = applyOrElse(key, (_:String) => otherwise)

  def isEmpty:Boolean = innerMap.isEmpty
  def nonEmpty = !isEmpty

  def dropIndexes = Props(dropIndexKeys(innerMap))

  def toFormattedString = formatted(0)
  private def escape(s: String) = s //*this part is good for debugging*/ map (c => ((c < 128) ? (""+c) | (c.toInt.formatted("\\u%04x") ))) mkString ""
  private def spaces(n:Int) =  " " * n
  private def quote(s:String) = Q+escape(s)+Q
  private def formatPf(pad:Int) = {
    val keysAndValues = innerMap.map(kv => (escape(kv._1), escape(kv._2))).toList.sorted
    val pairsFormatted = keysAndValues map { case(k,v) => "\"" + k + "\" -> \"" + v + "\""}
    pairsFormatted.mkString("Map(\n" + spaces(pad+2), "\n" + spaces(pad+2), "")
  }
  protected def formatted(pad: Int):String = spaces(pad) + (
    if (isEmpty) "Empty()" else "fp(\n" + formatPf(pad+2) + "\n" + spaces(pad)
    ) + ")"

  val Q = "\""

  private def toStringPf = {
    val keysAndValues = innerMap.map(kv => quote(kv._1) + " -> " + quote(kv._2)).toList.sorted

    "Map(" + keysAndValues.mkString(", ") + ")"
  }

  override def toString() = if (isEmpty) "Empty()" else s"fp($toStringPf)"

  override def equals(other:Any) = other match {
    case props: Props => props.innerMap == self.innerMap
    case _ => false
  }

  def getOr(key: Any, onError: Props => String) = Result(pfOpt("" + key), {
    onError(self)
  })

  def hasKey(key: String) = findKey(key).isDefined

  def @@ (key: Any) = getOr(key, props => {
    val k = key
    val howwasit = getOr(k, _=>"fck") //for testing
    s"Missing '$key' in $props"
  })
  def @@ (key: (Any, Any))          : Result[String] = @@ (""+key._1+"."+key._2)
  def @@ (key: (Any, Any, Any))     : Result[String] = @@ (""+key._1+"."+key._2+"."+key._3)
  def @@ (key: (Any, Any, Any, Any)): Result[String] = @@ (""+key._1+"."+key._2+"."+key._3+"."+key._4)
  def @?(key: String)(implicit defaultT: String) = Good(get(key).getOrElse(defaultT))

  def valueOf (key: Any)                 : Result[String] = @@(key)
  def valueOf (key: (Any, Any))          : Result[String] = @@(key)
  def valueOf (key: (Any, Any, Any))     : Result[String] = @@(key)
  def valueOf (key: (Any, Any, Any, Any)): Result[String] = @@(key)
  def guaranteedValueOf(key: String)(implicit defaultT: String) = @?(key)(defaultT)

  def translateBack(dictionary:Traversable[(String, String)]):Props = if (isEmpty) this else {
    val mapper = dictionary.toMap withDefault identity
    def remap(key: String) = {
      keyAsArray(key) map mapper mkString "."
    }
    Props(innerMap map { case(k,v) => remap(k) -> v })
  }

  def translate(dictionary:Traversable[(String, String)]):Props = if (isEmpty) this else {
    translateBack(dictionary.map(_.swap))
  }

  def translateBack(dictionary: Props):Props = translateBack(dictionary.innerMap)

  def translate(dictionary: Props):Props = translate(dictionary.innerMap)

  private def subtreeOfMap(map:PropMap, key: String):PropMap = {
    val subtreeOnThisLevel: PropMap = map.filterKeys(_.startsWith(key + ".")).map(kv => (kv._1.substring(key.length + 1), kv._2))
    val downOneLevel = dropPrefixInMap(map)
    val newMap = subtreeOnThisLevel ++ (downOneLevel.isEmpty ? downOneLevel | subtreeOfMap(downOneLevel, key))
    newMap
  }

  def subtree(key: String) = if (key.isEmpty) this else {
    Props(subtreeOfMap(innerMap, key))
  }

  private def dropPrefixInMap(map:PropMap) = {
    val mapWhereKeysAreSplitIntoPrefixAndTheRest = map map(kv => (kv._1.split("\\.", 2), kv._2))
    val mapWithDroppedPrefix = mapWhereKeysAreSplitIntoPrefixAndTheRest map (kv => (kv._1 drop 1 mkString, kv._2))
    val mapWithNonemptyKeys  = mapWithDroppedPrefix filter (kv => !kv._1.isEmpty)
    mapWithNonemptyKeys
  }

  def dropPrefix:Props = Props(dropPrefixInMap(innerMap))

  def dropAllPrefixes = Props({
    val withSplitKeys = innerMap.map(kv => (kv._1.split('.'), kv._2))
    val withGoodKeys = withSplitKeys.filter(kv => kv._1.nonEmpty)
    val withNewKeys =withGoodKeys.map(kv => (kv._1.last, kv._2))
    withNewKeys
  })

  def keySet:Set[String] = fullKeys map (_.split("\\.", 2)(0))

  def fullKeys:Set[String] = innerMap.keySet

  def commonKeys(other:Props) = fullKeys intersect other.fullKeys

  def trimPrefixes:Props =if (keySet.size == 1  && innerMap.keys.head.contains(".")) dropPrefix.trimPrefixes else self

  def findAllHaving(key: String) = filterKeys (key subsetOf _)

  def filterKeys(predicate: String => Boolean) = {
    val subMap = innerMap filterKeys predicate
    Props(subMap)
  }

  def findHaving(key:String) = findAllHaving(key).value(key) orCommentTheError s"key='$key'"

  def findHavingOneOf(variants:String*): Result[String] = {
    for (key <- variants) {
      val found = findAllHaving(key).value(key) orCommentTheError s"key='$key'"
      if (found) return found
    }
    Result.error(s"Failed to find unique entry for ${variants mkString ", "}")
  }

  def findAllIndexed:Props = {
    val subMap = innerMap.filterKeys(_ matches (sNumberKey + "\\..*"))
    Props(subMap)
  }

  def findAllHavingNumber(i: Int) = {
    val subMapWithIndexes = innerMap.filterKeys(_ matches (sNumberKey + "\\..*"))
    val atThisLevel = subMapWithIndexes filterKeys ( _ startsWith( numberKey(i)+"."))
    if (atThisLevel.nonEmpty || subMapWithIndexes.nonEmpty) Props(atThisLevel)
    else findAllHaving(numberKey(i))
  }

  private def extractCommonKeyValue(key: String): Result[String] = {
    val found: List[String] = fullKeys map {
      k => k.split("\\.").dropWhile(_ != key).toList.tail.headOption
    } collect {
      case Some(v) => v
    } toList

    found match {
      case Nil => Result.error("Not found")
      case (v:String)::Nil => Good(v)
      case more  => Result.error(s"Too many variants (${more.length}")
    }
  }

  private def value(key: String) = {
    val iterable: Iterable[String] = innerMap map (_._2)
    val values:Set[String] = iterable.toSet
    values.size match {
      case 0 => Result.error("No value found")
      case 1 => Good(values.head)
      case n =>
        val variants = values map (_.replaceAll(" ", ""))
        if (variants.size > 1) {
          extractCommonKeyValue(key)
//          Result.error(s"Found $n values, had to be just one")
        }
        else Good(values.head)
    }
  }

  def trimPrefixesWhile(p: String=>Boolean, collectedPrefixes:List[String] = Nil):(Props, String) = {
    if (keySet.size == 1 && p(keySet.head)) {
      val prefix = keySet.head
      val t = dropPrefix
      t.trimPrefixesWhile(p, prefix::collectedPrefixes)
    } else (self, collectedPrefixes.reverse mkString ".")
  }

  private def groupForIndexRange(indexRange: Range.Inclusive): Seq[Props] = {
    val result: Seq[Props] = indexRange map (i => {
      val key = numberKey(i)
      val havingThisNumber = findAllHavingNumber(i)
      val droppedUpToNumber = havingThisNumber.trimPrefixesWhile(key !=)._1
      droppedUpToNumber.dropPrefix
    })
    result
  }

  def extractIndexed: Seq[Props] = {
    val indexes = keySet collect { case NumberKeyPattern(n) => n.toInt }
    val indexRange = 1 to (if (indexes.isEmpty) 0 else indexes.max)
    if (indexes == indexRange.toSet) {
      groupForIndexRange(indexRange)
    } else this::Nil
  }

  def groupByIndex: Seq[Props] = {
    if (keySet forall isNumberKey) extractIndexed
    else this::Nil
  }

  def extractAllNumberedHaving(keys: String): Seq[Props] = {
    val containingKeys = findAllHaving(keys)
    val found = containingKeys.trimPrefixesWhile(key => !isNumberKey(key))
    val allIndexed = subtree(found._2)
    allIndexed groupByIndex
  }

  def addNumber(i: Int) = addPrefix(numberKey(i))

  def addPrefix(rawPrefix: String): Props = {
    val prefix = trimKey(rawPrefix)

    if (prefix.trim.isEmpty) this else {
      val newMap = innerMap map (kv => prefix+"."+kv._1 -> kv._2)
      Props(newMap)
    }
  }

  def reorder(paramsOrder:Int*):Props = if (isEmpty) this else {
    val keyMap = paramsOrder.zipWithIndex.toMap
    val newOrder = 1 to paramsOrder.size map keyMap

    def remap(key: String) = {
      val keys = key split "\\."
      if (keys.length < paramsOrder.length) key
      else newOrder map keys mkString "."
    }

    Props(innerMap map { case(k,v) => remap(k) -> v })
  }

  def addPrefixes(ps: Seq[String]): Props = if (isEmpty) self else ps.foldRight(self)((p, t) => t.addPrefix(p))

  private def checkForEmptyKeys():Unit = {
    if (keysWithEmptyValues.nonEmpty) {
      throw new IllegalArgumentException(s"Bad this: $self; check out keys $keysWithEmptyValues")
    }
  }

  def ++(that:Props): Props = {
    checkForEmptyKeys()
    that.checkForEmptyKeys()

    if (this.isEmpty) that else
    if (that.isEmpty) this else
      Props(self.innerMap ++ that.innerMap)
  }

  def ++(that: Map[String, String]): Props = ++(props(that))

  def endingWith(postfix: String): Props = Props(
    innerMap collect { case kv if kv._1.endsWith("." + postfix) =>
      val newLength = kv._1.length - postfix.length - 1
      (kv._1.substring(0, newLength), kv._2)
    })

  def startingWith(postfix: String): Props = Props(
    innerMap collect { case kv if kv._1.startsWith(postfix + ".") =>
      val newLength = kv._1.length - postfix.length - 1
      (kv._1.substring(0, newLength), kv._2)
    })

  // for testing
  def keysWithEmptyValues = fullKeys filter (apply(_).isEmpty)

  def transformKeys(keyTransformer: String=>String) = props(innerMap)(keyTransformer)
}

trait PropsOps {
  val Id:String=>String = conforms

  def numberKey(i: Int) = s"[[$i]]"

  def replaceAll(map: PropMap): String=>String = s => {
    map.keys.find(s matches).fold(s)(key => key.r.replaceAllIn(s, map(key)))
  }
  def replaceAll(mappings: (String, String)*): String=>String = replaceAll(mappings.toMap)
  type PropMap = Map[String, String]

  def accumulate(pp: TraversableOnce[Props]): Props = (Props.empty /: pp)(_++_)

  implicit def props(pf: Props): Props = pf

  def props(map: PropMap)(implicit keyTransformer: String=>String): Props = {
    if (map.isEmpty) Props.empty else {
      def transform(key: String): String = key.split("\\.").map(keyTransformer).mkString(".")
      val newMap = map.map(kv => (transform(kv._1), kv._2)) filter { case (k,v) => !k.isEmpty && !v.isEmpty}
      val result = Props(newMap)
      if (result.keysWithEmptyValues.nonEmpty) throw new IllegalArgumentException(s"bad map $newMap from $map")
      result
//      Props(newMap)
    }
  }

  def props(map: Map[_, _]): Props = props(map map {case (k,v) => k.toString->v.toString})

  def props(pairs: (String, String)*)(implicit keyTransformer: String=>String): Props = {
    props(pairs.toMap)(keyTransformer)
  }

  def propsFromTable(table: Iterable[_])(implicit keyTransformer: String=>String): Props = {
    val raw:PropMap = table.collect {
      case List(x, y)  => (x, y)
      case Array(x, y) => (x, y)
      case (x, y)      => (x, y)
    } .map ({case (k,v) => ("" + k) -> ("" + v)}).
      toMap[String, String]
    props(raw)(keyTransformer)
  }

  def seq2props(seq: List[_])(implicit keyTransformer: String=>String): Props = {
    val trimmed = seq .map (_.toString.replace(":", "").trim)
    val cleanedUp = ((List[String](), 'Value) /: trimmed) ((p, x) => {
      p match {
        case (list, 'Value) => if (x.isEmpty) p else (x::list, 'Key)
        case (list, 'Key)   => (x::list, 'Value)
      }
    })

    props(cleanedUp._1.reverse.grouped(2) .collect{
      case (pair:List[String]) if pair.length == 2 => pair(0).toLowerCase.replaceAll(" ", "") -> pair(1)
    }.toMap)(keyTransformer)
  }

  // opportunistically extract props from sequence of strings, some of them being empty etc
  def fromStrings(source: Seq[String], separator: String=":"): Props = {
    props(source map (_.split(separator, 2)) filter (_.length == 2) map (kv => kv(0).trim->kv(1).trim) toMap)
  }

  // opportunistically extract props from sequence of strings, some of them being empty etc
  def fromParallelLists(names: List[String], values: List[String], expected: List[String]): Result[Props] = {
    val missing = expected.map(_.toLowerCase).toSet diff names.map(_.toLowerCase).toSet
    val result: Result[Props] = Good(props(names zip values toMap))
    result.filter((p:Props) => missing.isEmpty, s"Missing column(s) (${missing mkString ","}) in ${names mkString ","}")
  }

  def fromList(source:List[String]) = propsFromTable(source.zipWithIndex map { case (a, i) => numberKey(i+1) -> a})

  def isPrimitive(x: Any) = x match {
    case u: Unit    => true
    case z: Boolean => true
    case b: Byte    => true
    case c: Char    => true
    case s: Short   => true
    case i: Int     => true
    case j: Long    => true
    case f: Float   => true
    case d: Double  => true
    case _          => false
  }

  def parsePair(k:Any, v:Any):Result[Props] = v match {
    case s:String            => Good(props(k.toString -> s))
    case x if isPrimitive(x) => Good(props(k.toString -> v.toString))
    case other               => fromTree(other) map(_ addPrefix k.toString)
  }

  def fromTree(source:Any):Result[Props] = source match {
    case l: List[_]  => Good(fromList(l map (_.toString)))
    case m: Map[_,_] =>
                        val pairs: TraversableOnce[Result[Props]] = m map (parsePair _).tupled
                        val result: Result[Traversable[Props]] = Result traverse pairs
                        result map accumulate
    case bs          => Result.error(s"Cannot extract properties from $bs")
  }
}

object Props extends PropsOps {

  def tryOr  [T](eval: =>T, onError: Exception => T) =
    try { eval } catch { case e: Exception => onError(e) }

  def tryOr  [T](eval: =>T, orElse: =>T) =
    try { eval } catch { case e: Exception => orElse }

  val sNumberKey = "\\[\\[(\\d+)\\]\\]"
  val NumberKeyPattern = sNumberKey.r
  lazy val empty = Props(Map.empty)
  val isNumberKey = (s:String) => s matches sNumberKey

//  def unapply(fp: Props): Option[PropMap] = Some(fp.innerMap)

  private val prefixSizeLimits = (1, 70)

  private def goodSize(s: String): Boolean = s.length >= prefixSizeLimits._1 && s.length <= prefixSizeLimits._2

  private def goodCharacter(s: String): Boolean = !("$0" contains s(0))

  private def trimKey(key: String) = {
    val found = key split "\n" find (!_.isEmpty) getOrElse ""
    val trimmed = found split "\\." map (_.trim) filter goodSize filter goodCharacter mkString "."
    trimmed
  }

  def fold(collection: TraversableOnce[Props]) = (empty /: collection)(_++_)

  private def isaLols(x: Any) = x match {
    case l: List[_] => l.forall({
      case ll: List[_] => ll.forall(_.isInstanceOf[String])
      case _ => false
    })
    case _ => false
  }

  // todo(vlad): civilize it
  def collectProps(source: Any): Result[Seq[Props]] = source match {
    case lols: List[_] if lols.forall(isaLols) =>
      val goodLols = lols.asInstanceOf[List[List[List[String]]]]
      val props = goodLols map propsFromTable
      Good(props)

    case x => Result.error(s"Failed to retrieve props from table: $x in $source")
  }

  private def sameKeyBundle(theKeyWeHave: String, suggestedAnyCase: String) = {
    val suggested = suggestedAnyCase.toLowerCase
    val ourKey = theKeyWeHave.toLowerCase

    ourKey == suggested || {
      val keys = ourKey.split("\\.").toSet
      val suggestedKeys = suggested.split("\\.") .map (_.replaceAll("\\?", ".")) .toSet

      keys == suggestedKeys
    }
  }

  private def dropIndexKeysInSequence(keys: Seq[String]) = keys map {
    case NumberKeyPattern(i) => ""
    case x => x
  } filter (!_.isEmpty)

  private def dropIndexKeys(map: PropMap) = {
    val withSplitKeys = map.map(kv => (kv._1.split('.'), kv._2))
    val cleanedUpKeys = withSplitKeys.map(kv => (dropIndexKeysInSequence(kv._1), kv._2))
    val withGoodKeys = cleanedUpKeys.filter(kv => kv._1.nonEmpty)
    val withNewKeys = withGoodKeys.map(kv => (kv._1 mkString ".", kv._2))
    withNewKeys
  }

  private def mapify(seq: Seq[Any]) = seq.zipWithIndex map {
    case (x, i) => numberKey(i+1) -> x
  } toMap

  private def fromMap(source: Map[_, _]): Props = {
    val collection: Iterable[Props] = source map {
      case (k0, v) =>
        val k = k0.toString
        v match {
          case null        => Props.empty
          case m:Map[_, _] => fromMap(m).addPrefix(k)
          case s:Seq[_]    => fromMap(mapify(s)).addPrefix(k)
          case x: Any      => props(k -> (""+x))
        }
    }
    accumulate(collection)
  }

  def parseJson(source: String): Result[Props] = {
    JSON.parseFull(source) match {
      case Some(stuff) =>
        stuff match {
          case m:Map[_, _] => Good(fromMap(m))
          case a:Seq[Any]         => Good(fromMap(mapify(a)))
          case x                  => Good(props("_" -> (""+x)))
        }
      case None => Result.error(s"wrong json: $source")
    }
  }

  val parse = new RegexParsers {
    override def skipWhitespace = false
    def number = "\\d+".r
    def numbers = "(" ~> repsep(number, ",") <~ ")"
    def text = "\"" ~> "[^\"]+".r <~ "\""
    def mapPair:Parser[(String, String)] = text ~ " -> " ~ text ^^ {
      case k ~ _ ~ v => k -> v
    }

    def mapContents:Parser[List[(String, String)]] = (mapPair ~ rep(",\\s*".r ~> mapPair)) ^^ {
      case h ~ t => h::t
    }

    // TODO: have tests pass with toFormattedString

    def eol: Parser[Any] = """(\r?\n)+""".r

    def mapExp: Parser[PropMap] = "Map\\(\\s*".r ~> mapContents <~ "\\s*".r <~ rep(eol) <~")" ^^ (_.toMap)

    def propMapExp: Parser[Props] = "fp\\(\\s*".r ~> mapExp <~ "\\s*".r <~ rep(eol) <~")" ^^ props

    def withDictionary = " with dictionary " ~> mapExp

    def withReorder = " with reordering " ~> numbers ^^ {_ map (_.toInt)}

    def optionally[X, Y](opt: Option[Y], f: Y => X => X): (X => X) = opt map f getOrElse identity[X]

    def propExp = propMapExp ~ (withDictionary ?) ~ (withReorder ?) ^^ {
      case props ~ dictionaryOpt ~ reorderOpt =>
        val withDictionary = (dictionaryOpt fold props) (props translate)
        (reorderOpt fold withDictionary) (withDictionary reorder)
    }

    def apply(s0: String) = {
      parseAll(propExp, s0) match {
        case Success(result, _) => Good(result)
        case NoSuccess(x, y) => Result.error("Failed to parse", x, y)
      }
    }
  }

  def keyMatches(suggestedAnyCase: String)(theKeyWeHave: String) = {
    val suggested = suggestedAnyCase.toLowerCase
    val ourKey = theKeyWeHave.toLowerCase

    ourKey == suggested || {
      val keySequence = ourKey.split("\\.").toList

      val suggestedSequence = suggested.split("\\.").toList map (_.replaceAll("\\?", "."))

      def match1(sample: String, segment:String):Boolean = tryOr(segment matches sample, false)

      def findNext(seq: List[String], sample: String) = seq.dropWhile(!match1(sample, _))

      val found = (keySequence /: suggestedSequence)((ks, sample) => findNext(ks, sample))

      found.nonEmpty
    }
  }

  def keyContains(suggestedAnyCase: String)(theKeyWeHave: String) = {
    val suggested = suggestedAnyCase.toLowerCase
    val ourKey = theKeyWeHave.toLowerCase

    ourKey == suggested || {
      val keySequence = ourKey.split("\\.").toList
      val suggestedSequence = suggested.split("\\.").toList map (_.replaceAll("\\?", "."))
      keySequence.length == suggestedSequence.length &&
        (
          keySequence zip suggestedSequence forall {
            case (key, s) => key contains s
          })
    }
  }
}


