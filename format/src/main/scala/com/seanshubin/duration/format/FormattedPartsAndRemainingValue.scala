package com.seanshubin.duration.format

case class FormattedPartsAndRemainingValue(formattedParts: List[Option[String]], remainingValue: Long) {
  private def divMod(numerator: Long, denominator: Long): (Long, Long) = (numerator / denominator, numerator % denominator)

  def applyTimeUnit(timeUnitAndQuantity: TimeUnitAndQuantity): FormattedPartsAndRemainingValue = {
    timeUnitAndQuantity match {
      case TimeUnitAndQuantity(timeUnit, Some(quantity)) =>
        val (newRemainingValue, partOfValueToFormat) = divMod(remainingValue, quantity)
        val formattedPart = timeUnit.format(partOfValueToFormat)
        copy(formattedPart :: formattedParts, newRemainingValue)
      case TimeUnitAndQuantity(timeUnit, None) =>
        val formattedPart = timeUnit.format(remainingValue)
        copy(formattedPart :: formattedParts, remainingValue)
    }
  }
}
