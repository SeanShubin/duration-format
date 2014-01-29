package com.seanshubin.duration.format

import scala.util.matching.Regex

class TimeUnitFormat(scale: List[TimeUnitAndQuantity]) {

  import TimeUnitFormat._

  def format(smallestUnits: Long): String = {
    def accumulateFormat(soFar: FormattedPartsAndRemainingValue, timeUnitAndQuantity: TimeUnitAndQuantity): FormattedPartsAndRemainingValue = {
      soFar.applyTimeUnit(timeUnitAndQuantity)
    }
    val initialValue = FormattedPartsAndRemainingValue(Nil, smallestUnits)
    val finalValue = scale.foldLeft(initialValue)(accumulateFormat)
    val formattedParts = finalValue.formattedParts.flatten
    if (formattedParts.isEmpty) "0 " + scale.head.timeUnit.plural
    else formattedParts.mkString(" ")
  }

  def parse(asString: String): Long = {
    if (asString.matches(NumberPattern)) {
      parseSimpleNumber(asString)
    } else if (asString.matches(OneOrMoreQuantifiedTimeUnitPattern)) {
      parseStringWithUnits(asString)
    } else {
      throw new RuntimeException(s"'$asString' does not match a valid pattern: $OneOrMoreQuantifiedTimeUnitPattern")
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

object TimeUnitFormat {
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
