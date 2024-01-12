package scalakittens

import scala.language.implicitConversions
import io.{Source => ioS}
import java.io._
import java.nio.channels.{Channels, ReadableByteChannel}

/**
  *
  * File system ops
  */
trait FS { fs =>

  implicit def asFile(file: TextFile):File              = file.file
  implicit def existingFile(file: File):   ExistingFile = new ExistingFile(file)
  implicit def existingFile(path: String): ExistingFile = existingFile(file(path))
  implicit def textFile(file: File):       TextFile     = new TextFile(file)
  implicit def textFile(path: String):     TextFile     = new TextFile(file(path))
  implicit def folder(file: File):         Folder       = Folder(file)
  implicit def folder(path: String):       Folder       = Folder(file(path))
  implicit def file(path: String): File                 = new File(path)
  def tempFile(prefix: String)                          = new TextFile(File.createTempFile(prefix, "tmp"))

  def probablyFile(path: String): Either[Any, ExistingFile] = try { Right(existingFile(file(path)))} catch { case x: Exception => Left(x) }

  def exists(file: File): Boolean = file.exists
  def exists(path: String): Boolean = exists(new File(path))

  // need the following so mocking could work
  def isDirectory(file: File): Boolean = file.isDirectory
  def isDirectory(path: String): Boolean = isDirectory(new File(path))

  sealed protected class Entry(val file: File) {
    require(file != null)

    lazy val canonicalFile = file.getCanonicalFile.getAbsoluteFile
    lazy val absolutePath = canonicalFile.getAbsolutePath
    def parent = Folder(canonicalFile.getParentFile)
    def delete = file.delete
    def exists = fs.exists(file)
    override def toString = canonicalFile.toString
    def size: Long = 0
  }

  class ExistingFile(f: File) extends TextFile(f) {
    require(exists, "File " + f + " must exist")
  }

  def isAbsolute(name: String) = {
    val sepAt = name indexOf File.separatorChar
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

    // TODO(vlad): stop returning and throwing; it's ancient
    def subfolder(name: String): Folder = {
      val tentative = Folder(file(name).file)
      if (tentative isSubfolderOf this) return tentative
      throw new IllegalArgumentException("Could not create subfolder '" + name + "' in '" + this + "'")
    }

    def existingFile(name: String) = new ExistingFile(file(name).file)
    def contains(name: String) = fs.exists(new File(path, name))
    def listFiles: List[File] = Option(canonicalFile.listFiles).toList.flatMap(_.toList)
    def files: List[ExistingFile] = listFiles.filter(_.isFile).map(f => existingFile(f.getName))
    def subfolders: List[Folder]  = listFiles.filter(_.isDirectory).map(Folder)
    def entries: List[Entry] = files ++ subfolders

    override def size: Long = entries.map(_.size).sum

    override def equals(x: Any) = x.isInstanceOf[Folder] && canonicalFile == x.asInstanceOf[Folder].canonicalFile
  }


  class TextFile(file: File) extends Entry(file) {
    override def size: Long = file.length

    def text = ioS.fromFile(file).mkString

    def text_=(content: AnyRef) {
      val out = new PrintWriter(file, "UTF-8")
      try{ out.print(content) } finally{ out.close() }
    }

    def text_+=(content: AnyRef) {
      val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(file, true), "UTF-8"))
      try{ out.print(content) } finally{ out.close() }
    }

    private def writeAt(os: FileOutputStream, pos: Long)(in: ReadableByteChannel, length: Long): Unit = {
      val out = os.getChannel
      try { out.transferFrom(in, pos, length) } finally { out.close() }
      ()
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

    // careful with this method, input stream may not show the full size via available()
    def <<<(in: InputStream) {
      write(in, in.available)
    }

    // careful with this method, input stream may not show the full size via available()
    def +<<(in: InputStream) {
      append(in, in.available)
    }

    def textOr(default: String) = try { text } catch { case _:Exception => default }
    def probablyText: Either[Any, String] = try { Right(text) } catch { case x: Exception => Left(x) }
  }

  // TODO(vlad): make it efficient - use channels
  def cp(from: TextFile, to: TextFile) { to.text = from.text }

  // For casual java usage
  def subfolder(parent: File, path: String) = folder(parent).subfolder(path).canonicalFile
}

object FS extends FS

class FSC extends FS {} // for Java