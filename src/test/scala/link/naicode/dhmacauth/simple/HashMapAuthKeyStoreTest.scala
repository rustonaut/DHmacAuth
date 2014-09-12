package link.naicode.dhmacauth.simple

import link.naicode.dhmacauth.Key
import org.specs2.mutable.Specification

/**
 * Created by naicode on 9/11/14.
 *
 */
class HashMapAuthKeyStoreTest extends Specification {

  def store = new HashMapAuthKeyStore(Map(
    0.toByte -> 1000,
    1.toByte -> 2000,
    3.toByte -> 3000
  ), 10)
  val key = Key(1,"",-1,1,0)

  "HashMapAuthDataQuery" should {

    "accept new keys" in {
      val nkey = store.setKey(key)
      (nkey.ident must_== 1) and
        (nkey.key must_== "") and
        (nkey.securityLevel must_== 0) and
        (nkey.timeout mustNotEqual -1) and
        (nkey.userId must_== 1)
    }

    "update the timeout" in {
      val str = store
      str.refreshTimeOutForKey(
        key.withTimeout(System.currentTimeMillis()+1000)
      ) must beSome
    }

    "dont update the timeout if timed out" in {
      val str = store
      str.refreshTimeOutForKey(key) must beNone
    }

    "return the correct timeout" in {
      store.timeoutForLevel(1) must be ~(System.currentTimeMillis()+2000 +/- 500)
    }
  }
}
