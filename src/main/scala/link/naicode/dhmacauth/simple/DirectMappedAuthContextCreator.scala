package link.naicode.dhmacauth.simple

import link.naicode.dhmacauth.AuthContextCreator

/**
 * Created by naicode on 9/10/14.
 *
 */
object DirectMappedAuthContextCreator extends AuthContextCreator[(Int, Byte)] {
  override def apply(userId: Int, level: Byte): (Int, Byte) = (userId, level)
}
