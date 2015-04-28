
package playfair

import scala.io.StdIn
import java.io.FileNotFoundException
import java.io.IOException

/**
 * Implementation of the Playfair Cipher encryption/decryption system.
 * @author keithdolan:kdolan03
 */

object Playfair {

  var password : String = ""
  var filename : String = ""
   
  def main(args: Array[String]) {
    
    while(true) {
      System.out.print("Enter Encode, Decode, or Quit (edq): ")
      var selection = StdIn.readLine()
    
      selection match  {
        case "e" => val pw = getUserInput("Password")
                    if (!pw.isEmpty()) {
                      val fn = getUserInput("Filename")
                      if (!fn.isEmpty()) {
                        readFile(fn) match {
                          case Some(plainText) => val coder = new Coder(pw)
                                                  System.out.println(coder.encode(plainText))
                          
                          case None =>  System.out.println("Unable to read input file: " + fn)
                        }
                      }
                    }
        
        case "d" => val pw = getUserInput("Password")
                    if (!pw.isEmpty()) {
                      val fn = getUserInput("Filename")
                      if (!fn.isEmpty()) {
                        readFile(fn) match {   
                          case Some(cipherText) => val coder = new Coder(pw)
                                                  System.out.println(coder.decode(cipherText))
                          
                          case None =>  System.out.println("Unable to read input file: " + fn)
                        }
                      }
                    }
        
        case "q" => System.exit(1)
        case _ => System.out.print("Selection " + selection + " is invalid. Select e,d or q only") 
      }
    }
  }
  
  
  /**
   * getUserInput: takes a prompt string and waits for
   * user input
   */
  
  def getUserInput(prompt: String) : String = {
    
    System.out.print("Enter " + prompt + ": ")
    val inStr = StdIn.readLine()
    if (inStr.isEmpty()) {
      System.out.print("No valid entry!")
    } 
    inStr
  }
  
  /**
   * readFile takes a filename and attempts to read
   * the entire file as a single string
   */
  
  def readFile(fileName : String) : Option[String] = {
    try {
      Some(scala.io.Source.fromFile(fileName).mkString)
    } catch {
      case ex: FileNotFoundException => System.out.println("Missing file exception");  None
      case ex: IOException => System.out.println("IO Exception"); None
      case _ : Throwable => None
    }
  }
}