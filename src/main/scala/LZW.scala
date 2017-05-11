object LZW {

  val initialDict = ('A' to 'Z').foldLeft(Map.empty[String, String])(
    (acc, letter) => acc + ("" + letter -> binaryString(letter - 'A')))

  val reverseVector: Vector[String] = ('A' to 'Z').map(_.toString).toVector

  def extractBiggestKey(key: String, word: Seq[Char], dict: Map[String, String]): (String, Seq[Char]) = word match {
      case Nil => (key, Nil)
      case x :: xs =>
        dict get (key + x) match {
        case None => (key, word)
        case _ => extractBiggestKey(key + x, xs, dict)
      }
    }

  def binaryString(pos: Int):String = {
    def rec(num: Int, acc: String): String = num match {
      case 0 => acc
      case n => rec(n / 2,(n % 2) + acc)
    }

    pos match {
      case 0 => "0"
      case n => rec(n, "")
    }
  }

  def fillToSize(str: String, size: Int): String = {
    if(str.length == size) str
    else fillToSize("0" + str, size)

  }

  def bitseqToInt(str: Seq[Char]) = str.foldLeft(0)((acc, x) => (acc << 1) + (if(x == '0') 0 else 1))

  def calculateSizeBits(size: Int) = ((Math.log(size) / Math.log(2)) + 1).toInt

  def encode(word: String): String = {
    def rec(acc: String, word: Seq[Char], dict: Map[String, String]): String = {
      word match {
        case Nil => acc
        case lst => val (key, rest) = extractBiggestKey("", lst, dict)
          rest match {
            case Nil => acc + fillToSize(dict(key), calculateSizeBits(dict.size))
            case _ => rec(
              acc + fillToSize(dict(key), calculateSizeBits(dict.size)),
              rest, dict + ((key + rest.head) -> binaryString(dict.size)))
          }
      }
    }
    rec("", word.toList, initialDict)
  }
  def decode(code: String): String ={
    def rec(acc: String, code: Seq[Char], vec: Vector[String]): String = {
      code match {
        case Nil => acc
        case _ =>
          val tam = calculateSizeBits(vec.size)
          val nextTam = calculateSizeBits(vec.size + 1)
          val (toDecode, rest) = code.splitAt(tam)
          rest match {
            case Nil => rec(acc + vec(bitseqToInt(toDecode)), rest, vec)
            case _ =>
              val nextLetter = bitseqToInt(rest.take(nextTam))
              val toAdd = if(nextLetter == vec.size) vec(bitseqToInt(toDecode)) else vec(nextLetter)
              rec(acc + vec(bitseqToInt(toDecode)),
              rest, vec :+ (vec(bitseqToInt(toDecode)) + toAdd.head))
          }


      }
    }
    rec("", code.toList, reverseVector)
  }
}
