package scalakittens

import scala.language.postfixOps
import management.ManagementFactory
import java.io.File
import io.Source
import scala.collection.JavaConverters._

trait OS {
  private val additionalPath = "/usr/local/bin:/bin"
  def myPid: String = ManagementFactory.getRuntimeMXBean.getName.split("@")(0)
  import sys.process._

  /**
    * @return short OS name
    */
  def uname: String = "uname" !!

  /**
    * @return true iff the system is Linux
    */
  def itsLinux: Boolean = uname contains "Linux"

  /**
    * @return user name
    */
  def whoami: String = System.getProperty("user.name")

  /**
    * @return canonical current path
    */
  def pwd: String = new File(".").getAbsoluteFile.getCanonicalPath

  def tmpDir: File = {
    val dir = new File("tmp")
    if (!dir.exists) dir.mkdir()
    if (!dir.isDirectory && dir.canRead && dir.canWrite) throw new Error("Failed to create tmp directory in " + pwd)
    dir
  }

  private def uniquePrefix = myPid + "." + Thread.currentThread.getId

  def tempFile(name: String, ext: String): File = {
    new File(tmpDir, uniquePrefix + "." + name + (if (ext.isEmpty) "" else "." + ext))
  }

  @deprecated(message="Use sys.process now", since="2015")
  def exec(cmd: Any*): Result[String] = {
    val env0 = Map[String, String]() ++ System.getenv().asScala
    val path = env0("PATH")
    val env = if (path.contains("/usr/local/bin")) env0 else env0 + ("PATH" -> (path + ":" + additionalPath))
    val envArray = env.map(p => p._1+"="+p._2).toArray
    val args: Array[String] = cmd.toArray.map(_.toString)

    val command = args mkString " "
    // the following solution is good for scala 2.9 usw - so why bother?
//    import scala.sys.process._
//    "convert -density 600 %s -monochrome %s".format(pdf, png) !
    val process = Runtime.getRuntime.exec(args, envArray)
    val code = process.waitFor
    if (code == 0) Good(command) else {
      val errorDump = Source.fromInputStream(process.getErrorStream).mkString
      Result.error(command + "\n" + errorDump)
    }
  }

}

object OS extends OS
