package scalakittens.web

import language.{implicitConversions, postfixOps, reflectiveCalls}

import scalakittens.{Good, Result, Strings}
import Strings._
import java.net.{HttpURLConnection, URL, URLDecoder, URLEncoder}
import org.apache.http.client.methods.{HttpPost, HttpGet}

sealed abstract class Url(val schema: String, private val __path: String) { self ⇒
  private lazy val (dom, base, page): (String, String, String) = {
    val p = __path.dropWhile('/'==)
    p.split("/").toList match {
      case d :: Nil ⇒
        ("//"+d, "", "")
      case d :: rest ⇒
        ("//"+d, rest.dropRight(1).mkString("/"), rest.last)
      case otherwise ⇒ (otherwise.head, otherwise.tail.mkString("/"), "") // this is a bad path anyway, who cares?
    }
  }

  private val WWW = "(/*www\\.)?(.+)".r

  def domain: String = dom match {
    case WWW(_, d) ⇒ d
    case otherwise ⇒ otherwise

  }

  private def javaUrl = new URL(toString)

  def openConnection(): Result[HttpURLConnection] = Result.attempt (
    javaUrl.openConnection() match {
      case huc: HttpURLConnection ⇒ Good(huc)
      case hmm ⇒ Result.error(s"Bad connection: $hmm for $self")
    },
    s"Failed to open connection: $self"
  )

  def httpCode: Result[Int] = {
    HttpURLConnection.setFollowRedirects(false)
    for {
      connection ← openConnection()
      _ = Result.forValue(connection.setRequestMethod("GET"))
      code ← Result.forValue(connection.getResponseCode)
    } yield code
  }

  def baseUrl = new SiteUrl(schema, s"$dom/$base")

  lazy val root = new SiteUrl(schema, dom)

  lazy val path = __path
  lazy val fullPath = {
    try {
      schema + ":" + path
    } catch {
      case x: Exception ⇒
        x.printStackTrace()
        "*** not found ***"
    }
  }

  def secure: Url

  def /(key: String, value: String): UrlWithParameters

  def /(kvs: (String, String)*): Url = {
    (this /: kvs)((url, kv) ⇒ url /(kv._1, kv._2))
  }

  def contains(s: String) = path containsIgnoreCase s

  def replace(key: String, oldValue: String, newValue: String) = this

  def param(key: String): Option[String] = None

  override def toString = fullPath

  override def equals(other: Any) = other match {
    case url: Url ⇒ this.toString == url.toString
    case _ ⇒ false
  }

  def GET = new HttpGet(fullPath)

  def POST = new HttpPost(fullPath)
}

/**
  * Deals with page addresses
  * Created by vpatryshev on 2/13/14.
  */
class SiteUrl(_schema:String, _path:String) extends Url(_schema, _path) {
  lazy val secure = new SiteUrl("https", path)
  def /(segment: String) = new SiteUrl(schema,
    if (segment startsWith "/") root.path + segment else path + "/" + segment)
  def /(key: String, value:String) = new UrlWithParameters(schema, _path, (key,value)::Nil)
}

protected class UrlWithParameters(_schema:String, _path:String, parameters:Seq[(String, String)]) extends SiteUrl(_schema, _path) {
  private def ue(s:String) = URLEncoder.encode(s, "UTF-8")
  lazy val paramMap = parameters.toMap
  override def param(key: String): Option[String] = paramMap.get(key)
  override lazy val secure = new UrlWithParameters("https", _path, parameters)
  lazy val paramsAsString = parameters map {case (k,v) ⇒ue(k) + "=" + ue(v)} mkString "&"
  override lazy val path = _path + (if(paramsAsString.isEmpty) "" else "?" + paramsAsString)
  override def /(key: String, value:String) = new UrlWithParameters(schema, _path, parameters :+ (key→value))

  override def replace(key:String, oldValue:String, newValue: String) = if (parameters.contains((key, oldValue))) {
    val index = parameters indexOf ((key, oldValue))
    val (before, after) = parameters splitAt index

    val newParameters = before ++ List((key,newValue)) ++ after.tail
    new UrlWithParameters(schema, _path, newParameters)
  } else this

}

object Url {
  implicit def asString(url:Url): String = url.fullPath

  def parameters(href: String): Map[String, String] = href split("?",2) toList match {
    case url::params::Nil ⇒ params split "&" map (_.split("=") toList) collect {
      case k::v::Nil ⇒ k→v
    } toMap

    case otherwise ⇒ Map.empty
  }

  val KeyValue = "(\\w+)=(.*)".r

  def apply(path0: String): SiteUrl = {
    val path = if (path0 endsWith "/") path0 dropRight 1 else path0
    if (path matches "\\w+:.*") {
      val su = path.split(":", 2)
      val hrefAndParams = su(1).split("\\?").toList
      hrefAndParams match {
        case h::p::Nil ⇒
          val params = URLDecoder.decode(p, "UTF-8") split "&" toList

          val keyValuePairs: List[(String, String)] = params collect {
            case KeyValue(key, value) ⇒ key → value
          }
          new UrlWithParameters(su(0), h, keyValuePairs)

        case otherwise ⇒ new SiteUrl(su(0), su(1))
      }
    } else
      new SiteUrl("http", path)
  }
}
