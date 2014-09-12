package link.naicode.utils.math

import scala.reflect.ClassTag

import scala.math.min

/**
 * Created by naicode on 8/30/14.
 *
 */

/**
 * provides a number of implicite converters needed for the
 * [[link.naicode.utils.math.HexString.fromParts()]] (and toParts)
 * method
 *
 * ==Usage==
 * {{{
 *   import link.naicode.utils.math.HexString
 *   import link.naicode.utils.math.Converters._
 *   val hstring = HexString.fromParts(Seq(0x12,0xAA,0xFE))
 *   assert(hstring.asString == "12AAFE")
 * }}}
 */
object Converters {
  implicit val h2Byte = (a:String) => HexString.parseInt(a).asInstanceOf[Byte]
  implicit val h2Int = (a:String) => HexString.parseInt(a)
  implicit val h2Long = (a:String) => HexString.parseLong(a)
  implicit val h2h = (a:String) => new HexString(a)
  implicit val h2Str = (a:HexString) => a.asString
  implicit val byte2h = (a:Byte) => Clamp.lenFillLeft(Integer.toHexString(a),2,'0')
  implicit val int2h = (a:Int) => Clamp.lenFillLeft(Integer.toHexString(a),8,'0')
  implicit val long2h = (a:Long) => Clamp.lenFillLeft(java.lang.Long.toHexString(a),16,'0')
}

/**
 * Provides a way to store a string known to contain only
 * hexadecimal characters and convert it to:
 *  - String,
 *  - Int,Long
 *  - Array of parts of Byte/Int/Long etc.
 * And parse it from all this types.
 *
 * Note that converting to some Type witch a given char length != the types size char length
 * will result in different data if converted back than before the conversation.
 *
 * ==Usage==
 * from "byte" like sequence:
 * {{{
 *   import link.naicode.utils.math.HexString
 *   import link.naicode.utils.math.Converters._
 *   val hstring = HexString.fromParts(Seq(0x12,0xAA,0xFE))
 *   assert(hstring.asString == "12AAFE")
 * }}}
 *
 * to byte array:
 * {{{
 *    import link.naicode.utils.math.HexString
 *    import link.naicode.utils.math.Converters._
 *    val hstring = HexString("10FF1F")
 *    val array = HexString.toParts[Byte](2)
 *    println(array)
 * }}}
 *
 * secure construction:
 * {{{
 *   HexString.tryConstruction("EQQQ...") match {
 *    case Some(hexStr) => println(hexStr)
 *    case None => println("incompatible"
 *   }
 * }}}
 *
 * @constructor only internally used use the companion object for construction
 * @param hstring a string from with it IS KNOWN that is is hexadecimal
 */
class HexString private[math](hstring:String) extends Equals{
  val str = hstring.toLowerCase

  /**
   * @return "HexString(theValue)"
   */
  override def toString: String = s"HexString($str)"

  /**
   * @return only the hex string itself without prefix/suffix or other decoration
   */
  def asString = str

  /** @return string length **/
  def length() = str.length

  /**
   * parses the hexstring as [[Int]].
   *
   * If the String is converted longer than 32Bit only
   * the lower 32Bit will be returned. The parser
   * will additionally treat the int as if unsigned:
   * {{{
   *   assert(HexString("FFFFFFFF").toInt == -1)
   * }}}
   * @return the int (partial)representation
   */
  def toInt = HexString.parseInt(str)

  /**
   * parses the hexstring as [[Long]].
   *
   * If the String is converted longer than 64Bit only
   * the lower 64Bit will be returned. The parser
   * will additionally treat the int as if unsigned
   *
   * @return the long (partial)representation
   */
  def toLong = HexString.parseLong(str)

  /**
   * Splits the HexString every `psize` Character
   * and converts each "part" to T
   *
   * This methode thorws if the String `length` is
   * not a mutiple of 2 or `psize` is smaler equal zero.
   *
   * Note if splitting into hexstring parts it is highly recomented to
   * use the converter supplied with [[link.naicode.utils.math.Converters]]
   * for efficiency reasons
   *
   * @param psize the Char size of the Data tyep
   * @param converter the String to T converter
   * @param ct class Tag needed to construct array
   * @tparam T the new type
   * @throws AssertionError if length()%2 != 0 or psize < 1
   * @return a array of type T representing this string
   */
  def toParts[T](psize:Int)(implicit converter: (String)=>T, ct:ClassTag[T]):Array[T] = {
    assert(length()%psize == 0)
    assert(psize > 0)

    (0 to (length()-psize,psize)).map { a: Int =>
      converter(str.substring(a, a + psize))
    }.toArray[T]
  }

  override def canEqual(that: Any): Boolean = that.getClass == this.getClass

  override def hashCode(): Int = str.hashCode

  override def equals(obj: scala.Any): Boolean = {
    if (obj.getClass == this.getClass) {
      obj.asInstanceOf[this.type].str.equals(this.str)
    } else false
  }


}

/**
 * this object is used for the creation of HexString's
 * it also supplys custom parseInt/Long methods
 */
object HexString {
  val hexStrRegex = "^(([0-9A-F]*)|([0-9a-f]*))$".r

  /**
   * converts a given string into a optional HexString
   * Note that mixing lower and uppercase Hexstrings is not allowed, but each for itself is
   * @param str the string
   * @return Some(HexString) if possible else None
   */
  def tryConstruction(str:String) = hexStrRegex.findFirstIn(str).map(new HexString(_))

  /**
   * converts a given string into a HexString failing if not possible
   * @param str the string
   * @return a HexString
   */
  def apply(str:String) = tryConstruction(str).get
  /** for pattern matching **/
  def unapply(hstr: HexString) = hstr.asString

  /**
   * create a HexString from a sequence of parts (e.g. Bytes)
   *
   * if this is not possible (e.g. the converters dont work)
   * this methode might throw a exception
   *
   * @param parts  the "parts"
   * @param reconverter how to converts each part to HexString before concatting them
   * @tparam T the parts type
   * @return  a valid HexString
   */
  def fromParts[T](parts:Seq[T])(implicit reconverter: (T)=> String): HexString = {
    val sb = new StringBuilder()
    HexString(parts.map(reconverter).foldLeft(sb)( (sb,b) => sb ++= b ).toString())
  }

  /**
   * Parses int from String (with only has hexadeciaml characters)
   *
   * A parse int from HexString variation witch is incompatible to
   * [[java.lang.Integer.parseInt(x,16)]]. The behaviour is best
   * described as "parse Long and then cast". Therefor:
   * {{{
   *   assert(HexString.parseInt("ffffffff") == -1)
   *   //but
   *   import util.Try
   *   assert(Try(Integer.parseInt("ffffffff",16)).isFailure)
   * }}}
   *
   * Note that you can emulate a parseByte/Short working the same way by using:
   * `parseInt(..).asInstanceOf[Byte]`
   * @param int the hexstring
   * @throws NumberFormatException if `int` is not a hexadecimal string
   * @return a int
   */
  def parseInt(int:String):Int = {
    val str = hexStrRegex.findFirstIn(int.toLowerCase)
      .getOrElse(throw new NumberFormatException(s"Illigal format for string=>int: $int"))
      .reverse
      .substring(0,min(8,int.length))
    var value:Int = 0
    var shift = 0
    for (ch <- str) {
      value += (if (ch >= 'a') ch-'a'+10 else ch-'0') << shift
      shift += 4
    }
    value
  }

  /**
   * Parses Long from String (with only has hexadeciaml characters)
   *
   * A parse Long from HexString variation witch is incompatible to
   * [[java.lang.Long.parseLong(x,16)]]. The behaviour is best
   * described as "parse something bigger then Long and the cast". Therefore:
   * {{{
   *   assert(HexString.parseInt("ffffffffffffffff") == -1)
   *   //but
   *   import util.Try
   *   assert(Try(java.lang.Long.parseLong("ffffffffffffffff",16)).isFailure)
   * }}}
   * @param long the hexstring
   * @throws NumberFormatException if `long` is not a hexadecimal string
   * @return a int
   */
  def parseLong(long:String):Long ={
    val str = hexStrRegex.findFirstIn(long.toLowerCase)
      .getOrElse(throw new NumberFormatException(s"Illigal format for string=>int: $long"))
      .reverse
      .substring(0,min(16,long.length))
    var value:Long = 0
    var shift = 0
    for (ch <- str) {
      value += (if (ch >= 'a') ch-'a'+10 else ch-'0').asInstanceOf[Long] << shift
      shift += 4
    }
    value
  }
}
