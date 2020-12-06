package com.github.meandor

class ValidatorSpec extends UnitSpec {
  Feature("isValidYear") {
    Scenario("is valid for 1920<=byr<=2002") {
      val values = Map(
        "byr" -> "2002"
      )

      val actual   = Validator.isValidYear("byr", 1920, 2002, values)
      val expected = true

      actual shouldBe expected
    }

    Scenario("is invalid for byr<1920") {
      val values = Map(
        "byr" -> "1919"
      )

      val actual   = Validator.isValidYear("byr", 1920, 2002, values)
      val expected = false

      actual shouldBe expected
    }

    Scenario("is invalid for foo>2002") {
      val values = Map(
        "foo" -> "2003"
      )

      val actual   = Validator.isValidYear("foo", 1920, 2002, values)
      val expected = false

      actual shouldBe expected
    }

    Scenario("is invalid for foo not 4 digits") {
      val values = Map(
        "byr" -> "1sa3"
      )

      val actual   = Validator.isValidYear("byr", 1920, 2002, values)
      val expected = false

      actual shouldBe expected
    }
  }

  Feature("isValidEyeColor") {
    val colors = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    colors.foreach { color =>
      Scenario(s"is valid for ecl=$color") {
        val values = Map(
          "ecl" -> color
        )

        val actual   = Validator.isValidEyeColor(values)
        val expected = true

        actual shouldBe expected
      }
    }

    Scenario("is invalid for 111") {
      val values = Map(
        "ecl" -> "111"
      )

      val actual   = Validator.isValidEyeColor(values)
      val expected = false

      actual shouldBe expected
    }
  }

  Feature("is valid passport-id") {
    Scenario("should return false for too short passport id") {
      val values = Map(
        "byr" -> "2002",
        "iyr" -> "2020",
        "eyr" -> "2030",
        "hgt" -> "193 cm",
        "hcl" -> "#123abc",
        "ecl" -> "blu",
        "pid" -> "00000001"
      )
      val passport = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = false

      actual shouldBe expected
    }

    Scenario("should return false for not only digits passport id") {
      val values = Map(
        "byr" -> "2002",
        "iyr" -> "2020",
        "eyr" -> "2030",
        "hgt" -> "193 cm",
        "hcl" -> "#123abc",
        "ecl" -> "blu",
        "pid" -> "00000001a"
      )
      val passport = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = false

      actual shouldBe expected
    }

    Scenario("should return false for too many digits passport id") {
      val values = Map(
        "byr" -> "2002",
        "iyr" -> "2020",
        "eyr" -> "2030",
        "hgt" -> "193 cm",
        "hcl" -> "#123abc",
        "ecl" -> "blu",
        "pid" -> "0000000111"
      )
      val passport = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = false

      actual shouldBe expected
    }
  }

  Feature("isValid") {
    Scenario("should return true for valid passport") {
      val values = Map(
        "byr" -> "2002",
        "iyr" -> "2020",
        "eyr" -> "2030",
        "hgt" -> "193 cm",
        "hcl" -> "#123abc",
        "ecl" -> "blu",
        "pid" -> "000000001"
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
        "byr" -> "2002",
        "iyr" -> "2020",
        "eyr" -> "2030",
        "hgt" -> "193 cm",
        "hcl" -> "#123abc",
        "ecl" -> "blu"
      )
      val passport = Passport(values)

      val actual   = Validator.isValid(passport)
      val expected = false

      actual shouldBe expected
    }
  }
}
