package org.scalakittens

import java.nio.ByteBuffer
import java.security.MessageDigest

/**
 * Contains md5 hash for data provided
 * @see <a href="en.wikipedia.org/wiki/MD5">en.wikipedia.org/wiki/MD5</a>
 */
class MD5(bytes: Array[Byte]) {
  private val digest = MessageDigest.getInstance("MD5")
  digest.reset()
  private def encode(b: Byte) = java.lang.Integer.toString(b & 0xff, 36)
  private val hash:(Long, Long) = {
    val buf = ByteBuffer.wrap(digest.digest(bytes))
    val first = buf.getLong
    val second = buf.getLong
    (first, second)
  }

  override def equals(x: Any): Boolean = x match {
    case other: MD5 => hash == other.hash
    case orelse     => false
  }

  override def hashCode(): Int = ((hash._1*31337 + hash._2) % 982451653).toInt // using good primes

  private def hex(l: Long) = "%08x".format(l)
  override def toString: String = hex(hash._1) + hex(hash._2)
}

object MD5 {
  def apply(bytes: Array[Byte]) = new MD5(bytes)
  def apply(message: String) = new MD5(message.getBytes("UTF-8"))
}
