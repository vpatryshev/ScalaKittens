package scalakittens

import io.{Source => ioS}
import java.io._

/**
 * File system ops
 */
trait FS {

  implicit def asFile(file: TextFile)                   = file.file
  implicit def existingFile(file: File):   ExistingFile = new ExistingFile(file)
  implicit def existingFile(path: String): ExistingFile = existingFile(file(path))
  implicit def textFile(file: File):       TextFile     = new TextFile(file)
  implicit def textFile(path: String):     TextFile     = new TextFile(file(path))
  implicit def folder(file: File):         Folder       = new Folder(file)
  implicit def folder(path: String):       Folder       = new Folder(file(path))
  implicit def file(path: String)                       = new File(path)
  def tempFile(prefix: String)                          = new TextFile(File.createTempFile(prefix, "tmp"))

  def probablyFile(path: String): Either[Any, ExistingFile] = try { Right(existingFile(file(path)))} catch { case x => Left(x) }

  def exists(file: File): Boolean = file.exists

  protected class Entry(val file: File) {
    require(file != null)

    lazy val canonicalFile = file.getCanonicalFile.getAbsoluteFile
    lazy val absolutePath = canonicalFile.getAbsolutePath
    def parent = new Folder(canonicalFile.getParentFile)
    def delete = file.delete
    override def toString = canonicalFile.toString
  }

  class ExistingFile(f: File) extends TextFile(f) {
    require(exists(f), "File " + f + " must exist")
  }

  case class Folder(path: File) extends Entry(path) {
    require(path != null)
    require(path.toString != "", "Path cannot be empty")
    require(!path.exists() || path.isDirectory, "Existing file " + path + " must be a directory")
    def /(name: String)    = new File(path, name)
    def file(path: Seq[String]): TextFile = new TextFile((canonicalFile /: path) (new File(_, _)))
    def file(path: String): TextFile = new TextFile(file(path split "/"))

    def isSubfolderOf(parent: Folder) = {
      absolutePath == parent.absolutePath ||
        (absolutePath startsWith (parent.absolutePath + File.separatorChar))
    }

    def subfolder(name: String): Folder = {
      if (!name.isEmpty) {
        try {
          val tentative = new Folder(name)
          if (tentative isSubfolderOf this) return tentative
        } catch {
          case _ => // ignore
        }
      }
      try {
        val tentative = new Folder(file(name).file)
        if (tentative isSubfolderOf this) return tentative
      }
      throw new IllegalArgumentException("Could not create subfolder '" + name + "' in '" + this + "'")
    }

    def existingFile(name: String) = new ExistingFile(file(name).file)
    def contains(name: String) = new File(path, name) exists
    def files: List[ExistingFile] = canonicalFile.listFiles filter (_.isFile) map (f => existingFile(f.getName)) toList
    def subfolders: List[Folder]  = canonicalFile.listFiles filter(_.isDirectory) map Folder toList

    override def equals(x: Any) = x.isInstanceOf[Folder] && canonicalFile == x.asInstanceOf[Folder].canonicalFile
  }

  class TextFile(file: File) extends Entry(file) {
    def text = ioS.fromFile(file).mkString

    def text_=(content: AnyRef) {
      val out = new PrintWriter(file, "UTF-8")
      try{ out.print(content) } finally{ out.close }
    }

    def text_+=(content: AnyRef) {
      val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(file, true), "UTF-8"))
      try{ out.print(content) } finally{ out.close }
    }

    def textOr(default: String) = try { text } catch { case _ => default }
    def probablyText: Either[Any, String] = try { Right(text) } catch { case x => Left(x) }
  }

  // TODO(vlad): make it efficient - use channels
  def cp(from: TextFile, to: TextFile) { to.text = from.text }

  // For casual java usage
  def subfolder(parent: File, path: String) = folder(parent).subfolder(path).canonicalFile
}

object FS extends FS

class FSC extends FS {} // for Java