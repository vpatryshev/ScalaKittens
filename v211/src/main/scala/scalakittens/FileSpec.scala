package scalakittens

import java.io.File
import util.matching.Regex

/**
 * Specifies files the way perforce does.
 * @see http://www.perforce.com/perforce/doc.current/manuals/cmdref/o.fspecs.html
 * Paths must be absolute only (so far)
 */
class FileSpec(val reg: Regex) {
  def accepts(file: File) = reg.findFirstMatchIn(file.getAbsolutePath).isDefined
  override def toString = reg.toString
}

object FileSpec {
  def apply(pattern: String) = new FileSpec(convertP4PatternToRegex(pattern))

  def convertP4PatternToRegex(s: String): Regex = {
    if (!s.matches("^[/*. \\w]+$")) throw new IllegalArgumentException("Oops in path pattern '" + s + "'")
    val s1 = s
      .replaceAll("\\*", "[^/]+")
      .replaceAll("\\.\\.\\.", "|")

    if (s1.matches(".[^ \\w]")) throw new IllegalArgumentException("Oops in path pattern '" + s + "'")
    ("^" + s1.replaceAll("\\.", "\\\\.")
      .replaceAll("\\|", ".+")
      + "$")
      .r
  }
}