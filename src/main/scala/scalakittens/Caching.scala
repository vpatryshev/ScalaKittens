package scalakittens

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.TimeUnit

/**
 */

trait Caching {

  protected def now: Long

  class CacheUnit[T](fun:() => T, timeout: Long) {

    private class Container(validUntil_nano: Long) {
      lazy val value: T = fun()
      def stillValid = now < validUntil_nano
    }

    private def update = new Container(now + timeout)
 
    private val ref = new AtomicReference[Container](update)

    def apply(): T = {
      val current = ref.get
      current.stillValid || ref.compareAndSet(current, update) // avoiding multiple resettings
      ref.get.value
    }
    
    def validFor(timeout: Long) = new {
      def NANOSECONDS  = apply(TimeUnit.NANOSECONDS)
      def MICROSECONDS = apply(TimeUnit.MICROSECONDS)
      def MILLISECONDS = apply(TimeUnit.MILLISECONDS)
      def SECONDS      = apply(TimeUnit.SECONDS)
      def MINUTES      = apply(TimeUnit.MINUTES)
      def HOURS        = apply(TimeUnit.HOURS)
      def DAYS         = apply(TimeUnit.DAYS)
      def apply(unit: TimeUnit) = new CacheUnit[T](fun, unit toNanos timeout)
    }
  }

  def cache[T](fun: () => T) = new CacheUnit[T](fun, Long.MaxValue)
}

object Caching extends Caching {
  protected def now = System.nanoTime
  
  implicit def currentValue[T](cu: CacheUnit[T]): T = cu.apply
}