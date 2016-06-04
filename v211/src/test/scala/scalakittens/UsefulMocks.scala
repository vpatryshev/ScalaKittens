package scalakittens

import java.io.{PrintStream, ByteArrayOutputStream, OutputStream}

trait UsefulMocks {
  def whatever[T] = throw new UnsupportedOperationException
  val MUTEX = "I am mutex, and what have you accomplished?"
  def sync(op: =>Unit): Unit = MUTEX synchronized op

  private val theirOut = System.out
/*
  def interceptOutput(op: => Unit) = sync {
    try {
      logPatience = Int.MaxValue
      val out = new OutputStream {
        val buf = new ByteArrayOutputStream()
        def write(b: Int) {
          b match {
            case '\r' => // ignore
            case '\n' => <<(buf.toString); buf.reset()
            case _ => buf.write(b)
          }
        }
      }
      val myOut: PrintStream = new PrintStream(out) {
        override def toString = s"Log buffer\n${testLog.mkString("\n")}\n"
      }
      System.setOut(myOut)
      op
    } finally {
      System.setOut(theirOut)
    }

  }*/
}

object UsefulMocks {

}