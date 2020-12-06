package com.github.meandor

object Validator {
  val requiredFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validEyeColors = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def isValidEyeColor(values: Map[String, String]): Boolean = {
    values.get("ecl").exists(validEyeColors.contains(_))
  }

  def isValidPassportId(values: Map[String, String]): Boolean = {
    val passportIdPattern = """\d{9}""".r
    values.get("pid").exists(passportIdPattern matches _)
  }

  def isInBetween(start: Int, end: Int)(value: Int): Boolean = {
    start <= value && value <= end
  }

  def isValidYear(value: String, start: Int, end: Int, values: Map[String, String]): Boolean = {
    val year: Option[String] = values.get(value)
    try {
      year.map(_.toInt).exists(isInBetween(start, end))
    } catch {
      case _: NumberFormatException => false
    }
  }

  def hasRequiredFields(passport: Passport): Boolean = {
    requiredFields.foldLeft(true) { (acc, element) =>
      acc & passport.values.contains(element)
    }
  }

  def isValid(passport: Passport): Boolean = {
    val passportValues = passport.values
    hasRequiredFields(passport) &&
    isValidYear("byr", 1920, 2002, passportValues) &&
    isValidYear("iyr", 2010, 2020, passportValues) &&
    isValidYear("eyr", 2020, 2030, passportValues) &&
    isValidEyeColor(passportValues) &&
    isValidPassportId(passportValues)
  }
}
