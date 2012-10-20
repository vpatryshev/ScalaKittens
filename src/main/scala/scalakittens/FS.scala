package scalakittens

import io.{Source => ioS}
import java.io._
import java.nio.channels.{Channels, ReadableByteChannel}

/**
 * File system ops
 */
trait FS { fs =>

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
  def exists(path: String): Boolean = exists(new File(path))

  // need the following so mocking could work
  def isDirectory(file: File): Boolean = file.isDirectory
  def isDirectory(path: String): Boolean = isDirectory(new File(path))

  protected case class Entry(val file: File) {
    require(file != null)

    lazy val canonicalFile = file.getCanonicalFile.getAbsoluteFile
    lazy val absolutePath = canonicalFile.getAbsolutePath
    def parent = new Folder(canonicalFile.getParentFile)
    def delete = file.delete
    def exists = fs.exists(file)
    override def toString = canonicalFile.toString
  }

  class ExistingFile(f: File) extends TextFile(f) {
    require(exists, "File " + f + " must exist")
  }

  def isAbsolute(name: String) = {
    var sepAt = name indexOf File.separatorChar
    sepAt == 0 ||
      sepAt > 0 && name(sepAt - 1) == ':'
  }

  case class Folder(path: File) extends Entry(path) {
    require(path != null)
    require(path.toString != "", "Path cannot be empty")
    require(!exists || isDirectory(path), "Existing file " + path + " must be a directory")
    def /(name: String)    = new File(path, name)
    def file(path: Seq[String]): TextFile = new TextFile((canonicalFile /: path) (new File(_, _)))
    def file(path: String): TextFile = new TextFile(file(path split "/"))
    def mkdirs = path.mkdirs

    def isSubfolderOf(parent: Folder) = {
      absolutePath == parent.absolutePath ||
        (absolutePath startsWith (parent.absolutePath + File.separatorChar))
    }

    def subfolder(name: String): Folder = {
      try {
        val tentative = new Folder(file(name).file)
        if (tentative isSubfolderOf this) return tentative
      }
      throw new IllegalArgumentException("Could not create subfolder '" + name + "' in '" + this + "'")
    }

    def existingFile(name: String) = new ExistingFile(file(name).file)
    def contains(name: String) = new File(path, name) exists
    def listFiles: List[File] = Option(canonicalFile.listFiles).toList.map(_.toList).flatten
    def files: List[ExistingFile] = listFiles filter (_.isFile) map (f => existingFile(f.getName)) toList
    def subfolders: List[Folder]  = listFiles filter(_.isDirectory) map Folder toList

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

    private def writeAt(os: FileOutputStream, pos: Long)(in: ReadableByteChannel, length: Long) {
      val out = os.getChannel
      try { out.transferFrom(in, pos, length) } finally { out.close }
    }

    def write(in: ReadableByteChannel, length: Long) {
      writeAt(new FileOutputStream(file, false), 0)(in, length)
    }

    def append(in: ReadableByteChannel, length: Long) {
      writeAt(new FileOutputStream(file, true), file.length)(in, length)
    }

    def write(in: InputStream, length: Long) {
      write(Channels.newChannel(in), length)
    }

    def append(in: InputStream, length: Long) {
      append(Channels.newChannel(in), length)
    }

    def <<<(in: InputStream) {
      write(in, in.available)
    }

    def +<<(in: InputStream) {
      append(in, in.available)
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