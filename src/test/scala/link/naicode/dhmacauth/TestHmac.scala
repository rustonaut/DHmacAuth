package link.naicode.dhmacauth

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import scala.util.Try


/**
 * Created by naicode on 9/11/14.
 *
 */
@RunWith(classOf[JUnitRunner])
class TestHmac extends Specification{

  "Hmac" should {

    "be construcktable for HmacSHA256" in {
      Try(Hmac("HmacSHA256")) must beSuccessfulTry
    }

    "hash the key/value pair correctly" in {
      Hmac("HmacSHA256").run("hallo","keykey").asString must_== "77ada60510b5635f2e8db7d73ffadfc0083be1db2afbff00d89d9aa51d813340"
    }

    "be construcktable for HmacMD5" in {
      Try(Hmac("HmacMD5")) must beSuccessfulTry
    }

    "hash the key/value pair correctly (MD5)" in {
      Hmac("HmacMD5").run("hallo","keykey").asString must_== "bbd5b214fc62d2d1969c0d1da6c4dd47"
    }
    "be construcktable for HmacMSHA1" in {
      Try(Hmac("HmacSHA1")) must beSuccessfulTry
    }

    "hash the key/value pair correctly (SHA1)" in {
      Hmac("HmacSHA1").run("hallo","keykey").asString must_== "e44c23d7b0279b09cc50ce9339d7bdcde23ace13"
    }
  }
}
