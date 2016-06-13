package scalakittens

import language.{implicitConversions, postfixOps}
import java.io._
import Ops._
import scala.io.{Codec, Source}
import java.net.URL
import java.nio.channels.Channels
import scala.util.Try
import scala.reflect.io.Streamable

trait IO { self ⇒
  implicit def asFile(path: String): File = new File(path)

  trait CanWrite {
    def #>(f: File): Result[File]
  }

  implicit def bytesWritable(buf: Array[Byte]): CanWrite = new CanWrite {
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

  implicit def stringWritable(s: String): CanWrite = bytesWritable(s.getBytes)


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

  def startsWith_old(prefix: Array[Byte])(file: File) =
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


  def onSource(s:Source)(op: String ⇒ Unit) = s.getLines().takeWhile(!_.isEmpty) foreach op

  def onStream(i: InputStream)(op: String ⇒ Unit) = onSource(Source.fromInputStream(i))(op)

  def onInput(op: String ⇒ Unit) = onStream(System.in)(op)

  def resource(url: String) = Result.forValue(self.getClass.getResource(url.replaceAll("%20", " "))) orCommentTheError s"Resource not found: $url"

  def fromResource(url: String) = resource(url) map (_.openStream)

  def readResource(path: String, codec:Codec = Codec.UTF8) = fromResource(path) map (in ⇒ {
    val text = Source.fromInputStream(in)(codec).mkString
    in.close()
    text
  })

  def readResourceBytes(path: String): Result[Array[Byte]] = fromResource(path) map (in ⇒ {
    val bytes = Streamable.bytes(in)
    in.close()
    bytes
  })

  def grabResource(path:String, codec:Codec = Codec.UTF8) =
    readResource(path, codec).getOrElse(throw new FileNotFoundException(s"Resource not found: $path"))

  private lazy val PropertyFormat = "([\\w\\.]+)\\b*=\\b*(.*)".r

  def propsFromSource(source: ⇒Source) = {
    var lines = tryOr(source.getLines().toList, List(""))

    lines.
      filter(line ⇒ !line.startsWith("#") && !line.isEmpty).
      collect { case PropertyFormat(key, value) ⇒ key->value}.
      toMap
  }

  def propsFromFile(filename: String) = propsFromSource(Source.fromFile(filename))

  def propsFromResource(path: String) = readResource(path) map (s ⇒ propsFromSource(Source.fromString(s))) getOrElse Map.empty

  implicit class BetterFile(val f:File) { self ⇒
    def extensionOpt:Option[String] = f.getName.split("\\.").toList match {
      case name::tail ⇒ tail.lastOption
      case Nil ⇒ None
    }
    def withExtension(ext:String) =
      new File(f.getParentFile,
              extensionOpt.fold(f.getName + ".")(f.getName dropRight _.length) + ext
      )

    def extension = extensionOpt getOrElse ""
    def extension_=(ext:String):Result[BetterFile] = {
      val g = self withExtension ext
      val newOne = if (f renameTo g) g else f
      if (newOne.extension == ext) Good(BetterFile(newOne)) else Result.error(s"Tried to set extension of $f to $ext, but failed")
    }
  }

  def saveToFile(s:String, name: String = "") = {
    Result.attempt ({
      val tmp = new File("tmp")
      tmp.mkdir
      if (!tmp.isDirectory) throw new IOException(s"Failed to create directory ${tmp.getAbsolutePath}")
      val file = new File(tmp, s"$name${System.currentTimeMillis}.html")
      s.getBytes #> file
      Good(file)
    })
  }
}

object IO extends IO
