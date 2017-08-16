package com.seanshubin.duration.format

import com.seanshubin.duration.format.DurationFormat.{MillisecondsFormat, MillisecondsFormatPadded}
import org.scalatest.FunSuite

class MillisecondsFormatTest extends FunSuite {
  test("parse") {
    assertParse("0", "0")
    assertParse("1 day", "86,400,000")
    assertParse("5 seconds", "5,000")
    assertParse("2 days", "172,800,000")
    assertParse("5 minutes", "300,000")
    assertParse("10 hours", "36,000,000")
    assertParse("1 second", "1,000")
    assertParse("1 millisecond", "1")
    assertParse("500 milliseconds", "500")
    assertParse("55 minutes", "3,300,000")
    assertParse("22", "22")
    assertParse("1 day 5 hours 2 minutes 1 second 123 milliseconds", "104,521,123")
    assertParse("2 Days 1 Hour 1 Minute 53 Seconds 1 Millisecond", "176,513,001")
    assertParse("32 days 5 hours", "2,782,800,000")
    assertParse("1 day 2 hours 1 day", "180,000,000")
    assertParse("1 hour 2 days 1 hours", "180,000,000")
    assertParse("25 days", "2,160,000,000")
    assertParse("9223372036854775807", "9,223,372,036,854,775,807")
    assertParse("9223372036854775807 milliseconds", "9,223,372,036,854,775,807")
    assertParse("106751991167 days 7 hours 12 minutes 55 seconds 807 milliseconds", "9,223,372,036,854,775,807")
  }

  test("parse trims whitespace") {
    assert(MillisecondsFormat.parse(" 1  minute ") === 60000)
  }

  test("back and forth") {
    assertBackAndForth("1 day 10 hours 17 minutes 36 seconds 789 milliseconds")
    assertBackAndForth("1 day 10 hours 36 seconds 789 milliseconds")
    assertBackAndForth("10 hours 17 minutes 36 seconds 789 milliseconds")
    assertBackAndForth("1 day 10 hours 17 minutes 36 seconds")
    assertBackAndForth("17 minutes")
    assertBackAndForth("789 milliseconds")
    assertBackAndForth("1 day 5 hours 2 minutes 1 second 123 milliseconds")
    assertBackAndForth("2 days 1 hour 1 minute 53 seconds 1 millisecond")
    assertBackAndForth("25 days")
    assertBackAndForth("0 milliseconds")
  }

  test("error message") {
    assertErrorMessage("1 foo", """'foo' does not match a valid time unit (milliseconds, seconds, minutes, hours, days)""")
    assertErrorMessage("1 SecondsA", """'SecondsA' does not match a valid time unit (milliseconds, seconds, minutes, hours, days)""")
    assertErrorMessage("a 1 foo", """'a 1 foo' does not match a valid pattern: \d+\s+[a-zA-Z]+(?:\s+\d+\s+[a-zA-Z]+)*""")
    assertErrorMessage("1 foo 3", """'1 foo 3' does not match a valid pattern: \d+\s+[a-zA-Z]+(?:\s+\d+\s+[a-zA-Z]+)*""")
    assertErrorMessage("seconds", """'seconds' does not match a valid pattern: \d+\s+[a-zA-Z]+(?:\s+\d+\s+[a-zA-Z]+)*""")
    assertErrorMessage("1 foo 2 bar", """'foo' does not match a valid time unit (milliseconds, seconds, minutes, hours, days)""")
  }

  test("order should not matter for parsing") {
    assert(MillisecondsFormat.parse("12 seconds 34 milliseconds") === 12034)
    assert(MillisecondsFormat.parse("34 milliseconds 12 seconds") === 12034)
  }

  test("duplicates get added together") {
    assert(MillisecondsFormat.parse("12 seconds 34 milliseconds 2 seconds") === 14034)
  }

  test("aligned") {
    assertAligned(0, "0 milliseconds")
    assertAligned(1, "  1 millisecond ")
    assertAligned(2, "  2 milliseconds")
    assertAligned(10, " 10 milliseconds")
    assertAligned(100, "100 milliseconds")
    assertAligned(999, "999 milliseconds")
    assertAligned(9223372036310399999L, "106751991160 days 23 hours 59 minutes 59 seconds 999 milliseconds")
    assertAligned(90061001, "1 day   1 hour   1 minute   1 second    1 millisecond ")
    assertAligned(180122002, "2 days  2 hours  2 minutes  2 seconds   2 milliseconds")
  }

  def assertParse(verbose: String, expected: String) {
    val parsed: Long = MillisecondsFormat.parse(verbose)
    val actual = f"$parsed%,d"
    assert(expected === actual)
  }

  def assertBackAndForth(verbose: String) {
    val parsed: Long = MillisecondsFormat.parse(verbose)
    val formatted = MillisecondsFormat.format(parsed)
    assert(verbose === formatted)
  }

  def assertErrorMessage(verbose: String, expected: String) {
    try {
      MillisecondsFormat.parse(verbose)
      fail(s"Expected '$verbose' to throw an exception during parsing")
    } catch {
      case ex: Exception =>
        assert(ex.getMessage === expected)
    }
  }

  def assertAligned(milliseconds: Long, expected: String): Unit = {
    val actual = MillisecondsFormatPadded.format(milliseconds)
    assert(actual === expected)
  }
}
