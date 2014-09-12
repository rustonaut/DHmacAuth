package link.naicode.dhmacauth.simple

import java.util.concurrent.ConcurrentHashMap

import link.naicode.dhmacauth.{Key, AuthKeyStore}


/**
 * Created by naicode on 9/10/14.
 *
 * A Simple ConcurrentHashMap based implementation of a AuthKeyStore
 * providing a necessary methods use DHmacAuthenticator in Memory
 * (but not necessarily the needed performance/loggin/distributivity/...
 * for a productive environment)
 */
class HashMapAuthKeyStore(val levelTimeoutMapping: Map[Byte,Long], val defaultTimeout: Long) extends AuthKeyStore {

  protected val intern = new ConcurrentHashMap[Int, Key]()

  override def queryForIdent(ident: Int): Option[Key] = {
    val key = intern.get(ident)
    if (key == null) None else Some(key)
  }

  def timeoutForLevel(level: Byte) = System.currentTimeMillis() + levelTimeoutMapping.getOrElse(level, defaultTimeout)

  /**
   * Updates the timeout for the given key an returns Some(key with new timeout)
   * or None if the Key already timed out
   *
   * Note this method is not thread save in the sense that:
   * two concurrent calls to refreshTimeout might each get the current timeout (not inside this function)
   * and then setting the new Key making one of them totally irrelevant.
   * Because the timeout is calculated in relation to the current time
   * this is not a problem.
   */
  override def refreshTimeOutForKey(key: Key): Option[Key] = {
    if (key.timeout >= System.currentTimeMillis()) {
      Some(setKey(key))
    } else None
  }

  override def setKey(key: Key): Key = {
    val newKey = key.withTimeout(timeoutForLevel(key.securityLevel))
    intern.put(key.ident, newKey)
    newKey
  }

}
