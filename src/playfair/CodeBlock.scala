package playfair

  /**
  * Take a keyword and create the codeblock. The block
  * is a 5x5 square and modeled as a List
  */

class CodeBlock (keyword : String ) {
  
  var alphabet = ('a' to 'z').filter(_ != 'j')
  var codeStr = ""
  
  keyword.map(i => if (!codeStr.contains(i.toLower)) codeStr += i.toLower)
  alphabet.map(i => if (!codeStr.contains(i)) codeStr += i)
  
  val codeblock = codeStr.toList
  
  /**
   * getPair takes a tuple of two characters, both in
   * lower case, and with any any j replaced with an i
   */
  
  def getPair (t: (Char, Char), encrypt: Boolean) : (Char, Char) = {
        
    val _1 = codeblock.indexOf(t._1)
    val _2 = codeblock.indexOf(t._2)
    
    var _1c = _1 % 5
    var _1r = _1 / 5
    var _2c = _2 % 5
    var _2r = _2 / 5
    
    // Allow for encryption and decryption
    var shift  = 1

    // Map for row, column or rectangle
    
    if (_1c == _2c) {             // Column 
      if (encrypt == false) {shift = -1; if (_1r == 0) _1r = 5; if (_2r == 0) _2r = 5}
      codeblock.apply((((_1r + shift) * 5) + _1c) % 25) -> codeblock.apply((((_2r + shift) * 5)  + _2c) % 25) 
    }
    else if (_1r == _2r) {        // Row
      if (encrypt == false) {shift = -1; if (_1c == 0 ) _1c = 5; if (_2c == 0 ) _2c = 5}
      codeblock.apply(((_1c + shift) % 5) + _1r * 5) -> codeblock.apply(((_2c + shift) % 5) + _2r * 5)
    }
    else {                        // Rectangle
      codeblock.apply((_1r * 5) + _2c) -> codeblock.apply((_2r * 5) + _1c)
    } 
  }
}