package com.seanshubin.duration.format

import scala.collection.mutable.ArrayBuffer

sealed abstract case class TimeUnit(singular: String, plural: String) {
  TimeUnit.valuesBuffer += this

  def format(value: Long): Option[String] =
    if (value == 0) None
    else if (value == 1) Some(s"$value $singular")
    else Some(s"$value $plural")

  def matchesString(target: String): Boolean = {
    singular.equalsIgnoreCase(target) || plural.equalsIgnoreCase(target)
  }
}

object TimeUnit {
  private val valuesBuffer = new ArrayBuffer[TimeUnit]
  lazy val values = valuesBuffer.toSeq

  val Nanosecond = new TimeUnit("nanosecond", "nanoseconds") {}
  val Microsecond = new TimeUnit("microsecond", "microseconds") {}
  val Millisecond = new TimeUnit("millisecond", "milliseconds") {}
  val Second = new TimeUnit("second", "seconds") {}
  val Minute = new TimeUnit("minute", "minutes") {}
  val Hour = new TimeUnit("hour", "hours") {}
  val Day = new TimeUnit("day", "days") {}
}
