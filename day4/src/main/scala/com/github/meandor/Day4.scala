package com.github.meandor
import scala.io.Source

object Day4 {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("usage: java -jar day4.jar PATH_TO_INPUT_FILE")
    } else {
      val file           = Source.fromFile(args.head)
      val lines          = file.getLines()
      val passports      = Parser.parse(lines)
      val validPassports = passports.filter(Validator.isValid)
      println(s"Found ${validPassports.length} valid passports")
      file.close()
    }
  }
}
