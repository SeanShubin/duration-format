package com.seanshubin.duration.format

import org.scalatest.FunSuite

class NanosecondsFormatTest extends FunSuite {
  test("parse") {
    assertParse("0", "0")
    assertParse("1 day", "86,400,000,000,000")
    assertParse("5 seconds", "5,000,000,000")
    assertParse("2 days", "172,800,000,000,000")
    assertParse("5 minutes", "300,000,000,000")
    assertParse("10 hours", "36,000,000,000,000")
    assertParse("1 second", "1,000,000,000")
    assertParse("1 millisecond", "1,000,000")
    assertParse("500 milliseconds", "500,000,000")
    assertParse("55 minutes", "3,300,000,000,000")
    assertParse("22", "22")
    assertParse("1 day 5 hours 2 minutes 1 second 123 milliseconds", "104,521,123,000,000")
    assertParse("2 Days 1 Hour 1 Minute 53 Seconds 1 Millisecond", "176,513,001,000,000")
    assertParse("32 days 5 hours", "2,782,800,000,000,000")
    assertParse("1 day 2 hours 1 day", "180,000,000,000,000")
    assertParse("1 hour 2 days 1 hours", "180,000,000,000,000")
    assertParse("25 days", "2,160,000,000,000,000")
    assertParse("9223372036854775807", "9,223,372,036,854,775,807")
    assertParse("9223372036854775807 nanoseconds", "9,223,372,036,854,775,807")
    assertParse("106751 days 23 hours 47 minutes 16 seconds 854 milliseconds 775 microseconds 807 nanoseconds", "9,223,372,036,854,775,807")
  }

  test("back and forth") {
    assertBackAndForth("1428 days 21 hours 33 minutes 9 seconds 123 milliseconds 456 microseconds 789 nanoseconds")
    assertBackAndForth("1 day 10 hours 36 seconds 789 milliseconds")
    assertBackAndForth("10 hours 17 minutes 36 seconds 789 milliseconds")
    assertBackAndForth("1 day 10 hours 17 minutes 36 seconds")
    assertBackAndForth("17 minutes")
    assertBackAndForth("789 milliseconds")
    assertBackAndForth("1 day 5 hours 2 minutes 1 second 123 milliseconds")
    assertBackAndForth("2 days 1 hour 1 minute 53 seconds 1 millisecond")
    assertBackAndForth("25 days")
    assertBackAndForth("0 nanoseconds")
  }

  test("error message") {
    assertErrorMessage("1 foo", """'foo' does not match a valid time unit (nanoseconds, microseconds, milliseconds, seconds, minutes, hours, days)""")
    assertErrorMessage("1 SecondsA", """'SecondsA' does not match a valid time unit (nanoseconds, microseconds, milliseconds, seconds, minutes, hours, days)""")
    assertErrorMessage("a 1 foo", """'a 1 foo' does not match a valid pattern: \d+\s+[a-zA-Z]+(?:\s+\d+\s+[a-zA-Z]+)*""")
    assertErrorMessage("1 foo 3", """'1 foo 3' does not match a valid pattern: \d+\s+[a-zA-Z]+(?:\s+\d+\s+[a-zA-Z]+)*""")
    assertErrorMessage("seconds", """'seconds' does not match a valid pattern: \d+\s+[a-zA-Z]+(?:\s+\d+\s+[a-zA-Z]+)*""")
    assertErrorMessage("1 foo 2 bar", """'foo' does not match a valid time unit (nanoseconds, microseconds, milliseconds, seconds, minutes, hours, days)""")
  }

  def assertParse(verbose: String, expected: String) {
    val parsed: Long = NanosecondsFormat.parse(verbose)
    val actual = f"$parsed%,d"
    assert(expected === actual)
  }

  def assertBackAndForth(verbose: String) {
    val parsed: Long = NanosecondsFormat.parse(verbose)
    val formatted = NanosecondsFormat.format(parsed)
    assert(verbose === formatted)
  }

  def assertErrorMessage(verbose: String, expected: String) {
    try {
      NanosecondsFormat.parse(verbose)
      fail(s"Expected '$verbose' to throw an exception during parsing")
    } catch {
      case ex: Exception =>
        assert(ex.getMessage === expected)
    }
  }
}
