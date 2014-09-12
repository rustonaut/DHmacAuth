package link.naicode.dhmacauth

/**
 * Created by naicode on 9/10/14.
 *
 * A Key containing all nedded information like the secret key, the
 * keys ident (for identification), the timeout, securityLevel and
 * corresponding user (identified by Id)
 */
case class Key(ident:Int, key:String, timeout:Long, userId:Int, securityLevel:Byte) {
  def withTimeout(newTimeout:Long) = Key(ident, key, newTimeout, userId, securityLevel)
}
