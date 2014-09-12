package link.naicode.dhmacauth

/**
 * Created by naicode on 9/10/14.
 *
 * The DHmacAuthenticator does not assumes anything from a
 * implementation of AuthKeyStore except:
 * 1. it is thread save (in the sense that it does not get corrupted or
 *  returns corrupted data when accessed multithreaded
 * 2. the methods are implemented accordingly to the documentation (mainly
 *  that it can depends on refreshTimeOutForKey returning None if the
 *  key already timed out!)
 *
 */
trait AuthKeyStore {

  /**
   * sets a (copy of) key and also sets timeout for the new key
   * returns the Key with timeout
   */
  def setKey(newKey: Key):Key

  /**
   * return the  Some(Key) for a given ident or None
   */
  def queryForIdent(ident: Int): Option[Key]

  /**
   * Updates the timeout for the given key and returns Some(key with new timeout)
   * or None if the Key already timed out
   *
   * If a timeout refresh is not supported it should return the same key
   * given to it (and maybe log a error/warning). A return value of None
   * may always be interpreted as that the key timed out!!
   */
  def refreshTimeOutForKey(key: Key): Option[Key]  = {
    if (key.timeout >= System.currentTimeMillis()) {
      Some(setKey(key))
    } else None
  }

}