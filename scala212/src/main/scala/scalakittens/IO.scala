package scalakittens

import language.{implicitConversions, postfixOps}
import java.io._
import scala.io.{Codec, Source}
import java.net.URL
import java.nio.channels.Channels
import scala.util.Try

trait IO { self =>
  implicit def asFile(path: String): File = new File(path)

  trait CanWrite {
    def #>(f: File): Result[File]
  }

  implicit def bytesWritable(buf: Array[Byte]): CanWrite = (f: File) => {
    val out = new FileOutputStream(f)
    val result = Result.attempt({ out.write(buf); Good(f) }, s"Failed to write to ${f.getAbsolutePath}")
    out.close()
    result
  }

  implicit def stringWritable(s: String): CanWrite = bytesWritable(s.getBytes)

  def using[T](file:File)(f: FileInputStream => T): Result[T] = Result(Try {
    val in = new FileInputStream(file)
    (Try (f(in)), Try (in.close()))._1
  } flatten)

  def startsWith(prefix: Array[Byte])(file: File):Boolean = {
    using (file) (in => { prefix forall (in.read == _) }) getOrElse false
  }

  val MinFileLength = 10

  def ensureFileIsOk(path: String): Result[File] = {
    Good(path).filter((path:String) => path.nonEmpty, "Empty file path, that's wrong.").
      flatMap(path => Result.attempt(ensureFileIsOk(new File(path).getAbsoluteFile.getCanonicalFile)))
  }

  /**
    * Check that a file is ok - it is file, it exists, and it is not too short
    * @param file the file
    * @return the same file wrapped in [[Result]]
    */
  def ensureFileIsOk(file: File):Result[File] = {
    Good(file).
      filter((_: File).getName.nonEmpty,       s"Empty file name, that's wrong.").
      filter((_:File).exists,                  f => s"File not found: $f").
      filter((_:File).isFile,                  f => s"Expected a file, but it is not: $f").
      filter((_:File).length >= MinFileLength, f => s"The file $f is too small (<$MinFileLength bytes).")
  }

  def copyToFile(url: URL, file: File): Unit = {
    file.delete()
    val out = new FileOutputStream(file).getChannel
    val in: InputStream = url.openStream()

    val ch = Channels.newChannel(in)
    try {
      while (in.available > 0) {
        val nBytes = in.available
        out.transferFrom(ch, out.position, nBytes)
        out.position(out.position + nBytes)
      }
    } finally { out.close() }
  }


  def onSource(s:Source)(op: String => Unit): Unit = s.getLines().takeWhile(_.nonEmpty) foreach op

  def onStream(i: InputStream)(op: String => Unit): Unit = onSource(Source.fromInputStream(i))(op)

  def onInput(op: String => Unit): Unit = onStream(System.in)(op)

  def resource(url: String): Result[URL] = Result.forValue(self.getClass.getResource(url.replaceAll("%20", " "))) orCommentTheError s"Resource not found: $url"

  def fromResource(url: String): Result[InputStream] = resource(url) map (_.openStream())

  def linesFromResource(path: String, codec:Codec = Codec.UTF8): Result[Iterator[String]] = fromResource(path) flatMap (in => Result.forValue {
    Source.fromInputStream(in)(codec).getLines()
  })
  
  def readResource(path: String, codec:Codec = Codec.UTF8): Result[String] = {
    linesFromResource(path, codec).map(_.mkString)
  }

  def bytesOf(inputStream: InputStream): Result[Array[Byte]] = Result.forValue {
    val in = new BufferedInputStream(inputStream)
    val it = Iterator continually in.read() takeWhile (-1 !=) map (_.toByte)
    it.toArray
  }

  def readResourceBytes(path: String): Result[Array[Byte]] = fromResource(path) flatMap (in => {
    val bytes = bytesOf(in)
    in.close()
    bytes
  })

  def grabResource(path:String, codec:Codec = Codec.UTF8): String =
    readResource(path, codec).getOrElse(throw new FileNotFoundException(s"Resource not found: $path"))

  implicit class BetterFile(val f:File) { self =>
    def extensionOpt:Option[String] = f.getName.split("\\.").toList match {
      case name::tail => tail.lastOption
      case Nil => None
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
