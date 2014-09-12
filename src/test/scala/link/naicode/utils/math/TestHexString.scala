package link.naicode.utils.math

/**
 * Created by naicode on 8/31/14.
 *
 */
import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._

import scala.util.Try
import Converters._

@RunWith(classOf[JUnitRunner])
class TestHexString extends Specification {

  "HexString" should {

    "return None if constructed with non hexstring" in {
      HexString.tryConstruction("gga") must beNone
    }
    "return None constructed with mixed upper/lower case hexstring" in {
      HexString.tryConstruction("fF") must beNone
    }

    "compform to apply/unapply" in {
      HexString.tryConstruction("AFF").get.asString must_== "aff"
    }

    "return lowercase hex values" in {
      HexString.unapply(HexString("AFF")) must_== "aff"
    }

    "fail if used incorectly" in {
      Try(HexString("aag")) must beFailedTry
    }

    "return the length on len" in {
      val a= "aab"
      HexString(a).length() must_== a.length
    }

    "have a working toInt methode" in {
      HexString("00FF").toInt must_== 0xFF
    }

    "cut bits if the number is to Large (int)" in {
      HexString("1FFFFFFFF").toInt must_== -1
    }

    "cut bits if the number is to Large (long)" in {
      HexString("1ffffffffffffffff").toInt must_== -1
    }

    "parseInt must parse correctly negative numbers" in {
      (HexString.parseInt("fffffffe") must_== -2) and
        (HexString.parseInt("ffffffff") must_== -1) and
        (HexString.parseInt("aff") must_== 0xaff)
    }
    "parseLong must parse correctly negative numbers" in {
      (HexString.parseLong("fffffffffffffffe") must_== -2) and
        (HexString.parseLong("ffffffffffffffff") must_== -1) and
        (HexString.parseLong("aff") must_== 0xaff)
    }

    "have a working toLong methode" in {
      HexString("1FFFFFFFFFF").toLong must_== 0x1ffffffffffL
    }

    "must be splittable into parts" in {
      HexString("aabbccdd").toParts[HexString](2).toList must containTheSameElementsAs(Seq(
          HexString("aa"),HexString("bb"),HexString("cc"),HexString("dd")))
    }

    "must be construcktable from parts" in {
      HexString.fromParts(Seq(HexString("aa"),HexString("BB"))).asString must_== "aabb"
    }

    "must be splittable into byte parts" in {
      HexString("ff00ff10ff").toParts[Byte](2).toList must containTheSameElementsAs(Seq(-1,0,-1,16,-1))
    }
    "must be construcktable from byte parts" in {
      HexString.fromParts[Byte](Seq(-1.toByte,0.toByte,16.toByte,-1.toByte)).asString must_== "ff0010ff"
    }

    "must be splittable into smale int parts" in {
      HexString("ff00ff10ff").toParts[Int](2).toList must containTheSameElementsAs(Seq(255,0,255,16,255))
    }
    "HAVE A DIFFERENT RESULT forFromParts when using a partsize != datatype size" in {
      HexString.fromParts(Seq(255,0,255,16,255)).asString must_!= "ff00ff10ff"
    }

    "must be splittable into byte parts" in {
      HexString("FFFFFFFE").toParts[Byte](8).toList must containTheSameElementsAs(Seq(-2))
    }
    "must be construcktable from int parts" in {
      HexString.fromParts(Seq(-2)).asString must_== "fffffffe"
    }

  }
}
