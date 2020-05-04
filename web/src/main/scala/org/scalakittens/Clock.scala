/*
 * Copyright 2010-2012 Health Expense, Inc.
 * All rights reserved
 */

package org.scalakittens

/**
 * Time micro-measurement made easier.
 *
 * TODO(vlad): inject mocking
 * TODO(vlad): see also DateAndTime.time(), a lot of similar things
 */

class Clock {
  private[this] var _startMillis = 0L
  private[this] var _stopMillis = _startMillis

  def start(): this.type = {
    _startMillis = System.currentTimeMillis()
    this
  }

  def stop(): this.type  = {
    _stopMillis  = System.currentTimeMillis()
    this
  }

  def dtMillis: Long = _stopMillis - _startMillis
  def dtMillisNow: Long = System.currentTimeMillis() - _startMillis

  def dtSec: Double = 0.001 * dtMillis

  def toStringMillis: String = "%s ms".format(dtMillis)
  def toStringSec: String = "%.3f sec".format(dtSec)
  def toStringUnitsPerSec(unitsName: String, units: Int): String = "%.3f %s/sec".format(units.toDouble / dtSec, unitsName)

  def isBeyondThreshold(millis: Long): Boolean = dtMillisNow > millis

  override def toString: String = toStringSec
}
