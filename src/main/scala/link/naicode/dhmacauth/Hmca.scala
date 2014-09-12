package link.naicode.dhmacauth

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import link.naicode.utils.math.HexString
import link.naicode.utils.math.Converters._

/**
 * Created by naicode on 9/10/14.
 *
 * A simple wrapper around the cryptographic methodes
 * provided by the JVM for Hmac.
 *
 * The `algo` parameter should be one of
 * - HmacSHA256
 * - HmacSHA1
 * - HmacMD5
 * neverless other cryptographic functions can be used to
 * (inclusive non HMAC algorithm)
 */
class Hmac(val algo:String) {
  val mac =  Mac.getInstance(algo)
  def run(msg:String, key:String):HexString = {
    mac.init(new SecretKeySpec(key.getBytes("utf-8"), algo))
    HexString.fromParts(mac.doFinal(msg.getBytes("utf-8")))
  }
}

object Hmac {
  def apply(algo: String) = new Hmac(algo)
}
