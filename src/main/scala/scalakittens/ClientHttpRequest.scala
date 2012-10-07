package scalakittens

import java.util.{Random, Observable}
import java.net.{URL, URLConnection}
import java.io._
import collection.mutable.ListBuffer

/**
 * <p>Title: Client HTTP Request class from MyJavaTools</p>
 * <p>Description: this class helps to send POST HTTP requests with various form data,
 * including files. Cookies can be added to be included in the request.</p>
 *
 * This class is a conversion from ClientHttpRequest.java v.6
 *
 * Licensed under the myjavatools license (Apache license version 2.0)
 * http://www.myjavatools.com/license.txt
 *
 * @author Vlad Patryshev
 * @author James Peltzer
 * @version 2.9.2
 */

class ClientHttpRequest extends Observable {
  private var connection: URLConnection = null
  lazy val os: OutputStream = connection.getOutputStream
  private val cookies = new ListBuffer[(String, String)]
  private var rawCookies = ""
  private var _bytesSent = 0
  def bytesSent = _bytesSent
  private var _filesSent = 0
  def filesSent = _filesSent
  var _isOpen = true

  protected def write(ss: String*) = {
    if (!_isOpen) throw new IOException("This request was already sent, too late to append.")
    ss foreach { s =>
      val bytes = s.getBytes
      os.write(bytes)
      _bytesSent += bytes.length
    }
    this
  }

  protected def newlineNumBytes: Long = {
    return 2
  }

  val CRLF = "\r\n"

  protected def newline = write(CRLF)

  protected def writeln(s: String) = write(s, CRLF)

  private var random: Random = new Random

  protected def randomString: String = {
    return java.lang.Long.toString(random.nextLong, 36)
  }

  private def boundaryNumBytes: Long = boundaryString.length + 2

  private var boundaryString: String = "---------------------------" + randomString + randomString + randomString

  private def writeBoundary = write("--", boundaryString)

  /**
   * Creates a new multipart POST HTTP request on a freshly opened URLConnection
   *
   * @param connection an already open URL connection
   * @throws IOException
   */
  def this(connection: URLConnection) {
    this()
    this.connection = connection
    connection.setDoOutput(true)
    connection.setDoInput(true)
    connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundaryString)
  }

  /**
   * Creates a new multipart POST HTTP request for a specified URL
   *
   * @param url the URL to send request to
   * @throws IOException
   */
  def this(url: URL) { this(url.openConnection) }

  /**
   * Creates a new multipart POST HTTP request for a specified URL string
   *
   * @param urlString the string representation of the URL to send request to
   * @throws IOException
   */
  def this(urlString: String) { this(new URL(urlString)) }

  private def postCookies {
    val newCookies = cookies map(c => c._1+"="+c._2) toList
    val all = (rawCookies :: newCookies) filterNot (_.isEmpty) mkString "; "
    if (!all.isEmpty) connection.setRequestProperty("Cookie", all)
  }

  /**
   * Sets cookies to the requst
   * @param rawCookies the full cookies string
   * @throws IOException
   */
  def setCookies(rawCookies: String) {
    this.rawCookies = rawCookies
    cookies.clear
  }

  /**
   * Adds a cookie to the requst
   * @param name cookie name
   * @param value cookie value
   * @throws IOException
   */
  def setCookie(name: String, value: String) {
    cookies += ((name, value))
  }

  /**
   * Adds cookies to the request
   * @param cookies array of cookie names and values
   * @throws IOException
   */
  def setCookies(cookies: Seq[(String, String)]) {
    this.cookies ++= cookies
  }

  /**
   * Adds cookies to the request
   * @param cookies array of cookie names and values
   * @throws IOException
   */
  def setCookie(cookies: (String, String)*) {
    this.cookies ++= cookies
  }

  private def writeNameNumBytes(name: String): Long = {
    return newlineNumBytes + "Content-Disposition: form-data; name=\"".length + name.getBytes.length + 1
  }

  private def writeName(name: String) = write(CRLF, "Content-Disposition: form-data; name=\"", name, "\"")

  private var isCanceled: Boolean = false

  def cancel {
    isCanceled = true
  }

//  def pipe(in: FileInputStream, out: OutputStream) {
//    val oc = Channels.newChannel(out)
//    val ic: FileChannel = in.getChannel
//    ic.transferTo(0, in.available, oc) // that's it? or should I
//  }

  private def pipe(in: InputStream, out: OutputStream) {
    val buf: Array[Byte] = new Array[Byte](ClientHttpRequest.BLOCK_SIZE)
    var nread: Int = 0
    while ({ nread = in.read(buf, 0, buf.length); nread >= 0 }) {
      out.write(buf, 0, nread)
      out.flush
      _bytesSent += nread
      if (isCanceled) {
        throw new IOException("Canceled")
      }
      this.setChanged
      this.notifyObservers(bytesSent)
      this.clearChanged
    }
  }

  /**
   * Adds a string parameter to the request
   * @param name parameter name
   * @param value parameter value
   * @throws IOException
   */
  def addParameter(name: String, value: String) = {
    writeBoundary
    writeName(name)
    newline
    newline
    writeln(value)
    this
  }

  /**
   * Adds a file parameter to the request
   * @param name parameter name
   * @param filename the name of the file
   * @param is input stream to read the contents of the file from
   * @throws IOException
   */
  def addFile(name: String, filename: String, is: InputStream): ClientHttpRequest = {
    writeBoundary
    writeName(name)
    write("; filename=\"", filename, "\"", CRLF, "Content-Type: ", contentType(filename), CRLF, CRLF)
    is match {
      case fis: FileInputStream => pipe(fis, os)
      case _ => pipe(is, os)
    }
    newline
    _filesSent += 1
    this
  }

  def contentType(filename: String): String = {
    Option(URLConnection.guessContentTypeFromName(filename)).getOrElse("application/octet-stream")
  }

  /**
   * Adds a file parameter to the request
   * @param name parameter name
   * @param file the file to upload
   * @throws IOException
   */
  def addFile(name: String, file: File): ClientHttpRequest = {
    val fis = new FileInputStream(file)
    try {
      addFile(name, file.getPath, fis)
    }
    finally {
      fis.close
    }
  }

  /**
   * Adds a parameter to the request; if the parameter is a File, the file is uploaded, otherwise the string value of the parameter is passed in the request
   * @param name parameter name
   * @param value parameter value, a File or anything else that can be stringified
   * @throws IOException
   */
  def addParameter(name: String, value: Any) {
    value match {
      case f: File => addFile(name, f)
      case _       => addParameter(name, value.toString)
    }
  }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added
   * @return input stream with the server response
   * @throws IOException
   */
  private def close: InputStream = {
    writeBoundary
    writeln("--")
    _isOpen = false
    os.close
    connection.getInputStream
  }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added
   * @return input stream with the server response
   * @throws IOException
   */
  def post: InputStream = { postCookies; close }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added before (if any), and with parameters that are passed in the argument
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setParameters
   */
  def post(parameters: (String, Any)*): InputStream = {
    postCookies
    parameters.foreach (p => addParameter(p._1, p._2))
    close
  }
}

object ClientHttpRequest {

  val BLOCK_SIZE = {
    val inceptionTime = 42
    val now = System.currentTimeMillis / 365.24 / 24 / 3600 / 1000
    val dt = now - inceptionTime
    (16000 * math.pow(2, dt / 3)).toInt // Moore's law
  }

  /**
   * Posts a new request to specified URL, with parameters that are passed in the argument
   * @param url: URL to send data to
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setParameters
   */
  def post(url: URL, parameters: (String, Any)*): InputStream = {
    new ClientHttpRequest(url).post(parameters: _*)
  }
}