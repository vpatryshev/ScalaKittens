package org.scalakittens

import java.io._
import java.net.URL
import java.nio.channels.Channels

import scala.io.{Codec, Source}
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.reflect.io.Streamable
import scala.util.Try

trait IO { self ⇒

  implicit def asFile(path: String): File = new File(path)

  implicit def bytesWritable(buf: Array[Byte]): Object {
    def #>(f: File): Result[File]
  } = new {
    /**
      * Writes an array of bytes to a file.
      * Has a side-effect
      * @param f Any [[File]]
      * @return [[Result]]
      */
    def #>(f: File): Result[File] = {
      val out = new FileOutputStream(f)
      val result = Result.attempt({out.write(buf); Good(f)}, s"Failed to write to ${f.getAbsolutePath}")
      out.close()
      result
    }
  }

  implicit def stringWritable(s: String): Object {
    def #>(f: File): Result[File]
  } = bytesWritable(s.getBytes)

  def withFile_old[T](file:File)(f: FileInputStream ⇒ T): Result[T] = {
    try {
      val in = new FileInputStream(file)
      try {
        Good(f(in))
      } catch {
        case x: Exception ⇒ Result.exception(x)
      } finally {
        try {in.close()}catch{case _:Any⇒ }
      }
    } catch {
      case y: Exception ⇒ Result.exception(y)
    }
  }

  def using[T](file:File)(f: FileInputStream ⇒ T): Result[T] = Result(Try {
    val in = new FileInputStream(file)
    (Try (f(in)), Try (in.close()))._1
  } flatten)

  def startsWith(prefix: Array[Byte])(file: File):Boolean = {
    using (file) (in ⇒ { prefix forall (in.read == _) }) getOrElse false
  }

  def startsWith(prefix: String)(file: File):Boolean = startsWith(prefix.getBytes)(file)

  def startsWith_old(prefix: Array[Byte])(file: File): Boolean =
    file.canRead &&
      withFile_old(file)(in ⇒ prefix forall (in.read ==)).toOption .exists (identity) 
  val MinFileLength = 10

  def ensureFileIsOk(path: String): Result[File] = {
    Good(path).filter((path:String) ⇒ !path.isEmpty, "Empty file path, that's wrong.").flatMap(path ⇒ Result.attempt(ensureFileIsOk(new File(path).getAbsoluteFile.getCanonicalFile)))
  }

  def ensureFileIsOk(f: File):Result[File] = {
    Good(f).
      filter(!(_:File).getName.isEmpty,              s"Empty file name, that's wrong.").
      filter( (_:File).exists,                  f ⇒ s"File not found: $f").
      filter( (_:File).isFile,                  f ⇒ s"Expected a file, but it is not: $f").
      filter( (_:File).length >= MinFileLength, f ⇒ s"The file $f is too small (<$MinFileLength bytes).")
  }

  lazy val HomeDir: File = new File(System.getProperty("user.home"))
  def pwd = new File(System.getProperty("user.dir"))

  def find(name: String, dir: File = pwd): Result[File] = {
    ensureFileIsOk(new File(dir, name)) orElse {
      val subdirs = dir.listFiles(new FileFilter {
        override def accept(path: File): Boolean = path.isDirectory
      })
      Result(subdirs.find(find(name, _).isGood)) map (new File(_, name))
    }
  }

  def copyToFile(url: URL, file: File) {
    file.delete()
    val out = new FileOutputStream(file).getChannel
    val in: InputStream = url.openStream

    val ch = Channels.newChannel(in)
    try {
      while (in.available > 0) {
        val nBytes = in.available
        out.transferFrom(ch, out.position, nBytes)
        out.position(out.position + nBytes)
      }
    } finally { out.close() }
  }


  def onSource(s:Source)(op: String ⇒ Unit): Unit = s.getLines().takeWhile(!_.isEmpty) foreach op

  def onStream(i: InputStream)(op: String ⇒ Unit): Unit = onSource(Source.fromInputStream(i))(op)

  def onInput(op: String ⇒ Unit): Unit = onStream(System.in)(op)

  def resource(url: String): Result[URL] = Result.forValue(self.getClass.getResource(url.replaceAll("%20", " "))) orCommentTheError s"Resource not found: $url"

  def fromResource(url: String): Result[InputStream] = resource(url) map (_.openStream)

  def readResource(path: String, codec:Codec = Codec.UTF8): Result[String] = fromResource(path) map (in ⇒ {
    val text = Source.fromInputStream(in)(codec).mkString
    in.close()
    text
  })

  def readResourceBytes(path: String): Result[Array[Byte]] = fromResource(path) map (in ⇒ {
    val bytes = Streamable.bytes(in)
    in.close()
    bytes
  })

  def grabResource(path:String, codec:Codec = Codec.UTF8): String =
    readResource(path, codec).getOrElse(throw new FileNotFoundException(s"Resource not found: $path"))

  def propsFromFile(filename: String): Props = Props.fromSource(Source.fromFile(filename))

  def propsFromResource(path: String):Props = readResource(path) map (s ⇒ Props.fromSource(Source.fromString(s))) getOrElse Props.empty

  implicit class BetterFile(val f:File) { self ⇒
    def extensionOpt:Option[String] = f.getName.split("\\.").toList match {
      case name::tail ⇒ tail.lastOption
      case Nil ⇒ None
    }
    def withExtension(ext:String) =
      new File(f.getParentFile,
        extensionOpt.fold(f.getName + ".")(f.getName dropRight _.length) + ext
      )

    def extension: String = extensionOpt getOrElse ""
    def extension_=(ext:String):Result[BetterFile] = {
      val g = self withExtension ext
      val newOne = if (f renameTo g) g else f
      if (newOne.extension == ext) Good(BetterFile(newOne)) else Result.error(s"Tried to set extension of $f to $ext, but failed")
    }
  }

  def saveToFile(s:String, name: String = ""): Result[File] = {
    Result.attempt ({
      val tmp = new File("tmp")
      tmp.mkdir
      if (!tmp.isDirectory) throw new IOException(s"Failed to create directory ${tmp.getAbsolutePath}")
      val file = new File(tmp, s"$name${System.currentTimeMillis}.html")
      s.getBytes #> file
      //      info(Good(file), s"HTML source saved to ${file.getAbsolutePath}")
      Good(file)
    })
  }
}

object IO extends IO
