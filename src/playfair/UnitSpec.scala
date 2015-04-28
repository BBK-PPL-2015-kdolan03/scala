package playfair

import org.scalatest._


abstract class UnitSpec extends FlatSpec {
  
  val pw = "Pennsylvania"
  val fn = "/Users/keithdolan/Documents/playfair1"
  
  Playfair.readFile(fn) match {
    case Some(plainText) => val coder = new Coder(pw)
      System.out.println(coder.encode(plainText))
                          
    case None =>  System.out.println("Unable to read input file: " + fn)
  }
  
  
  //System.out.println(coder.encode(plainText))
  
  //val fn2 = "/Users/keithdolan/Documents/pf_encrypted"

}