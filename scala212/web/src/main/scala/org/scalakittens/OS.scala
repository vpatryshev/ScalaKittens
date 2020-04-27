package org.scalakittens

import java.io.File
import java.lang.management.ManagementFactory

import scala.collection.JavaConverters._
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

trait OS {
  val additionalPath = "/usr/local/bin:/bin"
  def myPid: String = ManagementFactory.getRuntimeMXBean.getName.split("@")(0)
  import sys.process._
  def uname: String = "uname" !!
  def itsLinux: Boolean = uname contains "Linux"
  def whoami: String = System.getProperty("user.name")

//  def pwd = new File(".").getAbsoluteFile.getCanonicalPath

  def tmpDir: File = {
    val dir = new File("tmp")
    if (!dir.exists) dir.mkdir()
    if (!dir.isDirectory && dir.canRead && dir.canWrite) throw new Error("Failed to create tmp directory")
    dir
  }

  private def uniquePrefix = myPid + "." + Thread.currentThread.getId

  def tempFile(name: String, ext: String): File = {
    new File(tmpDir, uniquePrefix + "." + name + (if (ext.isEmpty) "" else "." + ext))
  }

  // deprecated? (message="Use sys.process now", "6/5/13")
  def exec(cmd: Any*): Result[String] = {
    val env0 = Map[String, String]() ++ System.getenv().asScala
    val path = env0("PATH")
    val env = if (path.contains("/usr/local/bin")) env0 else env0 + ("PATH" -> (path + ":" + additionalPath))
    val envArray = env.map(p ⇒ p._1+"="+p._2).toArray
    val args: Array[String] = cmd.toArray.map(_.toString)

    val command = args mkString " "

    val code = command !

    Good(command) filter((_:String) ⇒ code == 0, s"$command\n$code")
  }

}

object OS extends OS