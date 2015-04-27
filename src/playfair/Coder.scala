package playfair

import scala.util.matching.Regex
import annotation.tailrec

class Coder(keyword: String) {
  
  var codeblock = new CodeBlock(keyword)
  
  /**
   * formatOutput: Takes the imput string and format with a
   * space every NUM_BLOCKS and a newline every NUM_BLOCKS_PER_LINE
   */
  
  def formatOutput(inStr: String) : String = {
    
    var outStr : String = ""
    for (i <- 0 to inStr.length() - 1) {
      if (i % (Coder.NUM_BLOCKS * Coder.NUM_BLOCKS_PER_LINE)  == 0) { outStr += '\n'}
      else if (i % Coder.NUM_BLOCKS == 0) { outStr += ' '}
      outStr += inStr(i)
    }
    outStr
  }
  
  def encode(plainText: String): String = {
    
    val formatter = new InputFormatter(plainText)
    var il = formatter.getInput()
    
    def encrypt(l: List[Char]) : String = {
      @tailrec
      def pp(lc: List[Char], s: String ) : String = {
        if (lc.length == 0) s
        else
        {
          var a = lc.head
          var b = (lc.tail).head
          var ab = codeblock.getPair((a, b), true)
          pp(lc.tail.tail, s  + ab._1 + ab._2) 
        }
      }
      pp(l, "")
    }

    formatOutput(encrypt(il))
  }
  
  def decode(secretText: String): String  = {
    
    var noPunct = (for (i <- secretText if i.isLetter) yield i)
           
    def decrypt(st: String) : String = {
      var i = 0
      var outStr : String = ""
      while (i < st.length() - 1) {
        var ab = codeblock.getPair((st(i), st(i+1)), false)
        outStr = outStr + ab._1 + ab._2
        i = i + 2
      }
      outStr
    }

    // Replace "xqx" with "xx" and "_x_" with "__"
    
    var dec1 = decrypt(noPunct).replaceAll("xqx", "xx")
    var regex = "(.)x\\1".r
    var pos = (regex findAllMatchIn dec1 map (_.start)).toList
    
    @tailrec
    def removeNXN(str: String, pos: List[Int]) : String = {
      if (pos.length == 0) str
      else
      {
        var reg = str.substring(pos.head, pos.head + 3)
        var chr = str.apply(pos.head)
        var rep = (chr.toString() + chr.toString())
        var str1 = str.replace(reg, rep)
        removeNXN(str1, (regex findAllMatchIn str1 map (_.start)).toList)
      }
    }
    formatOutput(removeNXN(dec1, pos))
  }
}

object Coder {
  val NUM_BLOCKS = 5
  val NUM_BLOCKS_PER_LINE = 10
}