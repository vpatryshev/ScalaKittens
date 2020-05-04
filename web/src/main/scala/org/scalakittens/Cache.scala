package org.scalakittens

/**
  * Caches forever the values you produce, and gives them back on demand.
  */
trait Cache[K,V] {
  val produce: K ⇒ V
  def refresh(k:K): V
  def get(k: K): V
}

/**
  * This cache deal with pure functions and plain values.
  * Use it if you are sure your function does not fail.
  *
  * @param produce a function that produces values
  * @tparam K key type
  * @tparam V value type
  */
class FunctionCache[K,V](val produce: K ⇒ V) extends Cache[K, V] {
  private val storage = new scala.collection.mutable.HashMap[K, V]()

  def refresh(k:K): V = {
    val v = produce(k)
    storage(k) = v
    v
  }

  def get(k: K): V = storage getOrElse(k, refresh(k))
}

/**
  * This cache deal with partial functions and partial values.
  * Your producer may fail, and it returns a Bad then.
  * Use it if you suspect that your function may fail.
  *
  * Call refresh if you want to prepopulate the cache or update.
  *
  * @param produce produces a Result[V] for a given key,
  * @tparam K key type
  * @tparam V value type
  */
class PartialFunctionCache[K,V](val produce: K ⇒ Result[V]) extends Cache[K, Result[V]] {
  private val storage = new scala.collection.mutable.HashMap[K, Result[V]]()

  def refresh(k:K): Result[V] = {
    val rv = produce(k)
    storage(k) = rv
    rv
  }

  def get(k: K): Result[V] = {
    val fromStorage: Result[V] = Result(storage get k ).flatten
    fromStorage orElse refresh(k)
  }
}
