package com.seanshubin.duration.format

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object DurationFormat {

  object MillisecondsFormat extends TimeUnitFormat(TimeUnitAndQuantity.MillisecondToDay, shouldPad = false)

  object MillisecondsFormatPadded extends TimeUnitFormat(TimeUnitAndQuantity.MillisecondToDay, shouldPad = true)

  object NanosecondsFormat extends TimeUnitFormat(TimeUnitAndQuantity.NanosecondToDay, shouldPad = false)

  object NanosecondsFormatPadded extends TimeUnitFormat(TimeUnitAndQuantity.NanosecondToDay, shouldPad = true)

  private case class FormattedPartsAndRemainingValue(formattedParts: List[Option[String]], remainingValue: Long) {
    private def divMod(numerator: Long, denominator: Long): (Long, Long) = (numerator / denominator, numerator % denominator)

    def applyTimeUnit(timeUnitAndQuantity: TimeUnitAndQuantity, shouldPad: Boolean): FormattedPartsAndRemainingValue = {
      timeUnitAndQuantity match {
        case TimeUnitAndQuantity(timeUnit, Some(quantity)) =>
          val (newRemainingValue, partOfValueToFormat) = divMod(remainingValue, quantity)
          val formattedPart = timeUnit.format(partOfValueToFormat, shouldPad)
          copy(formattedPart :: formattedParts, newRemainingValue)
        case TimeUnitAndQuantity(timeUnit, None) =>
          val formattedPart = timeUnit.format(remainingValue, shouldPad)
          copy(formattedPart :: formattedParts, remainingValue)
      }
    }
  }

  private case class QuantityAndName(quantity: Long, timeUnit: TimeUnit) {
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

  sealed abstract case class TimeUnit(singular: String, plural: String, width: Int) {
    TimeUnit.valuesBuffer += this
    val padPolicyMap: Map[Boolean, PadPolicy] = Map(true -> ShouldPad, false -> ShouldNotPad)

    def format(value: Long, shouldPad: Boolean): Option[String] = {
      val padPolicy = padPolicyMap(shouldPad)
      if (value == 0) None
      else if (value == 1) Some(s"${padPolicy.padValue(value)} ${padPolicy.padSingular(singular)}")
      else Some(s"${padPolicy.padValue(value)} $plural")
    }

    def padIfNecessary(value: Long, shouldPad: Boolean): String = {
      if (shouldPad) {
        rightJustify(value.toString)
      } else {
        value.toString
      }
    }

    def rightJustify(value: String): String = {
      val spacesNeeded = width - value.length
      val spaces = " " * spacesNeeded
      spaces + value
    }

    def matchesString(target: String): Boolean = {
      singular.equalsIgnoreCase(target) || plural.equalsIgnoreCase(target)
    }

    trait PadPolicy {
      def padValue(value: Long): String

      def padSingular(singular: String): String
    }

    object ShouldPad extends PadPolicy {
      override def padValue(value: Long): String = rightJustify(value.toString)

      override def padSingular(singular: String): String = singular + " "
    }

    object ShouldNotPad extends PadPolicy {
      override def padValue(value: Long): String = value.toString

      override def padSingular(singular: String): String = singular
    }

  }

  private object TimeUnit {
    private val valuesBuffer = new ArrayBuffer[TimeUnit]
    lazy val values: Seq[TimeUnit] = valuesBuffer

    val Nanosecond = new TimeUnit("nanosecond", "nanoseconds", 3) {}
    val Microsecond = new TimeUnit("microsecond", "microseconds", 3) {}
    val Millisecond = new TimeUnit("millisecond", "milliseconds", 3) {}
    val Second = new TimeUnit("second", "seconds", 2) {}
    val Minute = new TimeUnit("minute", "minutes", 2) {}
    val Hour = new TimeUnit("hour", "hours", 2) {}
    val Day = new TimeUnit("day", "days", 0) {}
  }

  private case class TimeUnitAndQuantity(timeUnit: TimeUnit, maybeQuantity: Option[Int])

  private object TimeUnitAndQuantity {
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

  class TimeUnitFormat(scale: List[TimeUnitAndQuantity], shouldPad: Boolean) {

    import TimeUnitFormat._

    def format(smallestUnits: Long): String = {
      def accumulateFormat(soFar: FormattedPartsAndRemainingValue, timeUnitAndQuantity: TimeUnitAndQuantity): FormattedPartsAndRemainingValue = {
        soFar.applyTimeUnit(timeUnitAndQuantity, shouldPad)
      }

      val initialValue = FormattedPartsAndRemainingValue(Nil, smallestUnits)
      val finalValue = scale.foldLeft(initialValue)(accumulateFormat)
      val formattedParts = finalValue.formattedParts.flatten
      if (formattedParts.isEmpty) "0 " + scale.head.timeUnit.plural
      else formattedParts.mkString(" ")
    }

    def parse(asString: String): Long = {
      val trimmed = asString.trim
      if (trimmed.matches(NumberPattern)) {
        parseSimpleNumber(trimmed)
      } else if (trimmed.matches(OneOrMoreQuantifiedTimeUnitPattern)) {
        parseStringWithUnits(trimmed)
      } else {
        throw new RuntimeException(s"'$trimmed' does not match a valid pattern: $OneOrMoreQuantifiedTimeUnitPattern")
      }
    }

    private def parseSimpleNumber(asString: String): Long = {
      asString.toLong
    }

    private def parseStringWithUnits(asString: String): Long = {
      val parts = for {
        matchData <- QuantifiedTimeUnitCapturingRegex.findAllIn(asString).matchData
        numberString = matchData.group("number")
        nameString = matchData.group("name")
      } yield {
        val name = timeUnitFromString(nameString)
        val number = numberString.toLong
        val quantityAndName = QuantityAndName(number, name)
        quantityAndName.toUnitsAtScale(scale)
      }
      val sum = parts.sum
      sum
    }

    def timeUnitFromString(asString: String): TimeUnit = {
      val pluralNames = scale.map(_.timeUnit.plural).mkString("(", ", ", ")")

      def timeUnitMatches(timeUnit: TimeUnit): Boolean = timeUnit.matchesString(asString)

      TimeUnit.values.find(timeUnitMatches) match {
        case Some(timeUnit) => timeUnit
        case None => throw new RuntimeException(s"'$asString' does not match a valid time unit $pluralNames")
      }
    }
  }

  private object TimeUnitFormat {
    private val NumberPattern = """\d+"""
    private val NamePattern = """[a-zA-Z]+"""
    private val SpacesPattern = """\s+"""
    private val QuantifiedTimeUnitPattern = NumberPattern + SpacesPattern + NamePattern
    private val QuantifiedTimeUnitCapturingPattern = capturingGroup(NumberPattern) + SpacesPattern + capturingGroup(NamePattern)
    private val OneOrMoreQuantifiedTimeUnitPattern = QuantifiedTimeUnitPattern + nonCapturingGroup(SpacesPattern + QuantifiedTimeUnitPattern) + "*"
    private val QuantifiedTimeUnitCapturingRegex = new Regex(QuantifiedTimeUnitCapturingPattern, "number", "name")

    private def nonCapturingGroup(s: String) = "(?:" + s + ")"

    private def capturingGroup(s: String) = "(" + s + ")"
  }

}
