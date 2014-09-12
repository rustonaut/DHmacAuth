package link.naicode.utils.math

/**
 * Created by naicode on 8/31/14.
 *
 * Provides a number of clamp like functionality
 */
object Clamp {

  /**
   * clamps a string to a given length filling
   * `fill` chars at the right side if needed
   * (and also cutting the right side)
   * @param str orginal string
   * @param length to clamp to
   * @param fill the fill char
   * @return a string with given length
   */
  def lenFillRight(str: String, length: Int, fill: Char) = {
    if (str.length >= length) {
      str.substring(0,length)
    } else {
      val sb = new StringBuilder(length)
      sb ++= str
      while (sb.length < length) {
        sb += fill
      }
      sb.toString()
    }
  }

  /**
   *
   * clamps a string to a given length filling
   * `fill` chars at the left side if needed
   * (and also cutting the left side)
   * @param str orginal string
   * @param length to clamp to
   * @param fill the fill char
   * @return a string with given length
   */
  def lenFillLeft(str: String, length: Int, fill: Char) = {
    val strlen = str.length
    if (strlen >= length) {
      str.substring(strlen-length, strlen)
    } else {
      val diff = length - strlen
      val nfill = (new StringBuilder(diff) += fill) * diff
      nfill + str
    }
  }

  /**
   * creates a function clamping to the given range (inklusive min/max)
   * There is a specialised version for Byte,Short,Char,Int,Long,Double,Float to
   * prevent unnecessary Boxing/Unboxing
   * Note @specialised does not necessarily what is expected
   * @param min minimal (inclusive) number
   * @param max maximal (inclusive) number
   * @tparam T a Ordered type
   * @return a number inside the range [min;max]
   */
  def range[T <: Ordered[T]](min: T, max: T): (T) => T = {
    return (x:T) => {
      if (x < min) min
      else if (x > max) max
      else x
    }
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Int, max: Int):(Int)=>Int = {
    x => if(x<min)min else if (x>max) max else x
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Byte, max: Byte):(Byte)=>Byte = {
    x => if(x<min)min else if (x>max) max else x
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Long, max: Long):(Long)=>Long = {
    x => if(x<min)min else if (x>max) max else x
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Double, max: Double):(Double)=>Double = {
    x => if(x<min)min else if (x>max) max else x
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Float, max: Float):(Float)=>Float= {
    x => if(x<min)min else if (x>max) max else x
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Char, max: Char):(Char)=>Char = {
    x => if(x<min)min else if (x>max) max else x
  }

  /**specailised version of [[link.naicode.utils.math.Clamp.range()]] **/
  def range(min: Short, max: Short):(Short)=>Short= {
    x => if(x<min)min else if (x>max) max else x
  }

}
