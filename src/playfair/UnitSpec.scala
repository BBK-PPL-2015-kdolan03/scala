package playfair

import org.scalatest._


abstract class UnitSpec extends FlatSpec with Matchers {
  
  "An InputFormatter" should "remove whitespace and punctuation" in {
    val formatter = new InputFormatter("AbbCDE ; ,, ooOx")
    var il = formatter.getInput().mkString("")
    il.compare("abbcdeoxoxox") should be (0)
  }
}