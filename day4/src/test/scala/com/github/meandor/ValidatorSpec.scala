package com.github.meandor

class ValidatorSpec extends UnitSpec {
  Feature("isValid") {
    Scenario("should return true for valid passport") {
      val values = Map(
        "byr" -> "foo",
        "iyr" -> "foo",
        "eyr" -> "foo",
        "hgt" -> "foo",
        "hcl" -> "foo",
        "ecl" -> "foo",
        "pid" -> "foo"
      )
      val passport = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = true

      actual shouldBe expected
    }

    Scenario("should return false empty passport") {
      val values: Map[String, String] = Map()
      val passport                    = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = false

      actual shouldBe expected
    }

    Scenario("should return false for one missing required field passport") {
      val values = Map(
        "byr" -> "foo",
        "iyr" -> "foo",
        "eyr" -> "foo",
        "hgt" -> "foo",
        "hcl" -> "foo",
        "ecl" -> "foo"
      )
      val passport = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = false

      actual shouldBe expected
    }
  }
}
