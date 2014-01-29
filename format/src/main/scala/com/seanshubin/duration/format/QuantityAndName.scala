package com.seanshubin.duration.format

case class QuantityAndName(quantity: Long, timeUnit: TimeUnit) {
  def toUnitsAtScale(fullScale: List[TimeUnitAndQuantity]): Long = {
    val scale = fullScale.takeWhile(_.timeUnit != timeUnit)
    def accumulateByMultiply(soFar: Long, timeUnitAndQuantity: TimeUnitAndQuantity): Long = {
      timeUnitAndQuantity.maybeQuantity match {
        case Some(currentQuantity) => soFar * currentQuantity
        case None => throw new RuntimeException(s"No multiplier for ${timeUnitAndQuantity.timeUnit.plural}")
      }
    }
    val units = scale.foldLeft(quantity)(accumulateByMultiply)
    units
  }
}
