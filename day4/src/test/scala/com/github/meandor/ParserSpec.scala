package com.github.meandor

class ParserSpec extends UnitSpec {
  Feature("merge maps") {
    Scenario("should merge empty map and non empty one") {
      val m1: Map[String, String] = Map()
      val m2                      = Map("a" -> "b")

      val actual   = Parser.merge(m2, m1)
      val expected = Map("a" -> "b")

      actual shouldBe expected
    }

    Scenario("should combine m1 and m2") {
      val m1 = Map("b" -> "c")
      val m2 = Map("a" -> "b")

      val actual   = Parser.merge(m1, m2)
      val expected = Map("b" -> "c", "a" -> "b")

      actual shouldBe expected
    }

    Scenario("should overwrite existing key from m1 with m2") {
      val m1 = Map("a" -> "a")
      val m2 = Map("a" -> "b", "b" -> "c")

      val actual   = Parser.merge(m1, m2)
      val expected = Map("a" -> "b", "b" -> "c")

      actual shouldBe expected
    }
  }

  Feature("parse line") {
    Scenario("single key value pair") {
      val keyValuePair = "eyr:2024"

      val actual   = Parser.parse(keyValuePair)
      val expected = Map("eyr" -> "2024")

      actual shouldBe expected
    }
    Scenario("n key value pairs") {
      val keyValuePair = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"

      val actual = Parser.parse(keyValuePair)
      val expected = Map(
        "ecl" -> "gry",
        "pid" -> "860033327",
        "eyr" -> "2020",
        "hcl" -> "#fffffd"
      )

      actual shouldBe expected
    }
  }

  Feature("parse lines") {
    Scenario("should parse single passport") {
      val actual = Parser.parse(Iterator("foo:bar foobar:foo", "bar:foo"))
      val values = Map(
        "foo"    -> "bar",
        "foobar" -> "foo",
        "bar"    -> "foo"
      )
      val expected = Seq(Passport(values))

      actual shouldBe expected
    }

    Scenario("should parse n passports") {
      val actual =
        Parser.parse(Iterator("foo:bar foobar:foo", "", "bar:foo", "foo:bar", "", "foo:bar"))
      val expected = Seq(
        Passport(
          Map(
            "foo"    -> "bar",
            "foobar" -> "foo"
          )
        ),
        Passport(
          Map(
            "bar" -> "foo",
            "foo" -> "bar"
          )
        ),
        Passport(
          Map(
            "foo" -> "bar"
          )
        )
      )

      actual should contain theSameElementsAs expected
    }
  }
}
