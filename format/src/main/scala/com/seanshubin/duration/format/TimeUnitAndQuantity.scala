package com.seanshubin.duration.format

case class TimeUnitAndQuantity(timeUnit: TimeUnit, maybeQuantity: Option[Int])

object TimeUnitAndQuantity {
  val MillisecondToDay =
    TimeUnitAndQuantity(TimeUnit.Millisecond, Some(1000)) ::
      TimeUnitAndQuantity(TimeUnit.Second, Some(60)) ::
      TimeUnitAndQuantity(TimeUnit.Minute, Some(60)) ::
      TimeUnitAndQuantity(TimeUnit.Hour, Some(24)) ::
      TimeUnitAndQuantity(TimeUnit.Day, None) ::
      Nil
  val NanosecondToDay =
    TimeUnitAndQuantity(TimeUnit.Nanosecond, Some(1000)) ::
      TimeUnitAndQuantity(TimeUnit.Microsecond, Some(1000)) ::
      MillisecondToDay
}
