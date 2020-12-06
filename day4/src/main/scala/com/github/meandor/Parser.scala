package com.github.meandor

object Parser {
  def merge(m1: Map[String, String], m2: Map[String, String]): Map[String, String] = {
    (m1.toSeq ++ m2).toMap
  }

  def parse(line: String): Map[String, String] = {
    val pattern                    = """((\w+):(\S+))+""".r
    val matches                    = (pattern findAllIn line).toSeq
    val value: Map[String, String] = Map.empty
    matches.foldLeft(value) { (acc, element) =>
      val pair = element.split(":")
      acc + (pair(0) -> pair(1))
    }
  }

  def parse(lines: Iterator[String]): Seq[Passport] = {
    val value: Seq[Passport] = Seq(Passport(Map.empty))
    lines.foldLeft(value) { (acc, line) =>
      if (line == "") {
        acc.+:(Passport(Map.empty))
      } else {
        val newValues             = merge(acc.head.values, parse(line))
        val passportWithNewValues = Passport(newValues)
        acc.tail.+:(passportWithNewValues)
      }
    }
  }
}
