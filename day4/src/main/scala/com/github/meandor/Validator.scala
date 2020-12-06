package com.github.meandor

object Validator {

  val requiredFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def isValid(passport: Passport): Boolean = {
    requiredFields.foldLeft(true) { (acc, element) =>
      acc & passport.values.contains(element)
    }
  }
}
