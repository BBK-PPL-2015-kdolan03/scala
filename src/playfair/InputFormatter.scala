package playfair

/**
 * Parse the input file string to remove all punctuation
 * and all white space, change to lower case, and replace
 * any 'j' with 'i'. Note that numerals are not supported
 * in the Playfair system
 */
class InputFormatter (input : String) { 
  
  var lower = (for (i <- input if i.isLetter) yield i).map(x => x.toLower)
  var loweri = lower.map(x => if (x == 'j') 'i' else x)
  
  /**
   * Function to append to the generated list and insert
   * the filler character if required.
   */
  
  def formatFunc(a: List[Char], b: Char) : List[Char] = {
    var filler = 'x'
    if (a.isEmpty) {
      List(b)
    } 
    else 
    {
      a.last match {
        case `b` => if (a.length %2 == 1) {
                      if (b == 'x') {filler = 'q'}  
                      a ::: List(filler) ::: List(b) 
                    }
                    else { a ::: List(b) }
        case _ => a ::: List(b)
      } 
    }
  }
    
  /**
   * Create a List of characters with any fillers needed
   * between any pairs
   */

  val inputList = loweri.foldLeft(List[Char]())(formatFunc)
  
  def getInput() : List[Char] = {
    inputList
  }
}