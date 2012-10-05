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

  protected def write(ss: String*) = {
    ss foreach { s => os.write(s.getBytes) }
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
  private var _bytesSent: Int = 0

  def bytesSent = _bytesSent

  def cancel {
    isCanceled = true
  }

  private def pipe(in: InputStream, out: OutputStream) {
    var buf: Array[Byte] = new Array[Byte](1024)
    var nread: Int = 0
    _bytesSent = 0
    isCanceled = false
    in synchronized {
      while ((({
        nread = in.read(buf, 0, buf.length); nread
      })) >= 0) {
        out.write(buf, 0, nread)
        _bytesSent += nread
        if (isCanceled) {
          throw new IOException("Canceled")
        }
        out.flush
        this.setChanged
        this.notifyObservers(bytesSent)
        this.clearChanged
      }
    }
    out.flush
    buf = null
  }

  /**
   * Adds a string parameter to the request
   * @param name parameter name
   * @param value parameter value
   * @throws IOException
   */
  def setParameter(name: String, value: String) {
    writeBoundary
    writeName(name)
    newline
    newline
    writeln(value)
  }

  /**
   * Adds a file parameter to the request
   * @param name parameter name
   * @param filename the name of the file
   * @param is input stream to read the contents of the file from
   * @throws IOException
   */
  def setParameter(name: String, filename: String, is: InputStream) {
    writeBoundary
    writeName(name)
    write("; filename=\"", filename, "\"", CRLF, "Content-Type: ", contentType(filename), CRLF, CRLF)
    pipe(is, os)
    newline
  }


  def contentType(filename: String): String = {
    Option(URLConnection.guessContentTypeFromName(filename)).getOrElse("application/octet-stream")
  }

  def getFilePostSize(name: String, file: File): Long = {
    val filename: String = file.getPath
    return boundaryNumBytes + writeNameNumBytes(name) + "; filename=\"".length + filename.getBytes.length + 1 + newlineNumBytes + "Content-Type: ".length + contentType(filename).length + newlineNumBytes + newlineNumBytes + file.length + newlineNumBytes
  }

  /**
   * Adds a file parameter to the request
   * @param name parameter name
   * @param file the file to upload
   * @throws IOException
   */
  def setParameter(name: String, file: File) {
    var fis: FileInputStream = null
    try {
      fis = new FileInputStream(file)
      setParameter(name, file.getPath, fis)
    }
    finally {
      if (fis != null) {
        fis.close
      }
    }
  }

  /**
   * Adds a parameter to the request; if the parameter is a File, the file is uploaded, otherwise the string value of the parameter is passed in the request
   * @param name parameter name
   * @param value parameter value, a File or anything else that can be stringified
   * @throws IOException
   */
  def setParameter(name: String, value: Any) {
    value match {
      case f: File => setParameter(name, f)
      case _       => setParameter(name, value.toString)
    }
  }

  /**
   * Adds parameters to the request
   * @param parameters "name-to-value" map of parameters; if a value is a file, the file is uploaded, otherwise it is stringified and sent in the request
   * @throws IOException
   */
  def setParameters(parameters: Seq[(String, Any)]) {
    parameters.foreach (p => setParameter(p._1, p._2))
  }

//  /**
//   * Adds parameters to the request
//   * @param parameters (vararg) parameter names and values (parameters[2*i] is a name, parameters[2*i + 1] is a value); if a value is a file, the file is uploaded, otherwise it is stringified and sent in the request
//   * @throws IOException
//   */
//  def setParameters(parameters: (String, Any)*) {
//    parameters foreach { p => setParameter(p._1, p._2) }
//  }

  def getPostFooterSize: Long = {
    return boundaryNumBytes + 2 + newlineNumBytes + newlineNumBytes
  }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added
   * @return input stream with the server response
   * @throws IOException
   */
  private def doPost: InputStream = {
    writeBoundary
    writeln("--")
    os.close
    connection.getInputStream
  }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added
   * @return input stream with the server response
   * @throws IOException
   */
  def post: InputStream = { postCookies; doPost }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added before (if any), and with parameters that are passed in the argument
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setParameters
   */
  def post(parameters: Seq[(String, Any)]): InputStream = {
    postCookies
    setParameters(parameters)
    doPost
  }

//  /**
//   * Posts the requests to the server, with all the cookies and parameters that were added before (if any), and with parameters that are passed in the argument
//   * @param parameters request parameters
//   * @return input stream with the server response
//   * @throws IOException
//   * @see setParameters
//   */
//  def post(parameters: (String, Any)*): InputStream = {
//    postCookies
//    setParameters(parameters:_*)
//    doPost
//  }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added before (if any), and with cookies and parameters that are passed in the arguments
   * @param cookies request cookies
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setParameters
   * @see setCookies
   */
  def post(cookies: Seq[(String, String)], parameters: Seq[(String, Any)]): InputStream = {
    setCookies(cookies)
    postCookies
    setParameters(parameters)
    doPost
  }

  /**
   * Posts the requests to the server, with all the cookies and parameters that were added before (if any), and with cookies and parameters that are passed in the arguments
   * @param raw_cookies request cookies
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setParameters
   * @see setCookies
   */
  def post(raw_cookies: String, parameters: Seq[(String, Any)]): InputStream = {
    setCookies(raw_cookies)
    postCookies
    setParameters(parameters)
    doPost
  }
}

object ClientHttpRequest {

  /**
   * Posts a new request to specified URL, with parameters that are passed in the argument
   * @param url: URL to send data to
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setParameters
   */
  def post(url: URL, parameters: Seq[(String, Any)]): InputStream = {
    new ClientHttpRequest(url).post(parameters)
  }

//  /**
//   * Posts a new request to specified URL, with parameters that are passed in the argument
//   * @param parameters request parameters
//   * @return input stream with the server response
//   * @throws IOException
//   * @see setParameters
//   */
//  def post(url: URL, parameters: (String, Any)*): InputStream = {
//    new ClientHttpRequest(url).post(parameters:_*)
//  }

  /**
   * Posts a new request to specified URL, with cookies and parameters that are passed in the argument
   * @param cookies request cookies
   * @param parameters request parameters
   * @return input stream with the server response
   * @throws IOException
   * @see setCookies
   * @see setParameters
   */
  def post(url: URL, cookies: Seq[(String, String)], parameters: Seq[(String, Any)]): InputStream = {
    new ClientHttpRequest(url).post(cookies, parameters)
  }
}
