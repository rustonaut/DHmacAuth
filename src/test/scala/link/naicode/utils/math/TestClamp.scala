package link.naicode.utils.math

/**
 * Created by naicode on 8/31/14.
 *
 */

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestClamp extends Specification {

  "Clamp.range" should {

    "clamp to big numbers" in {
      Clamp.range(-2,10)(11) must_== 10
    }
    "clamp to smal numbers" in {
      Clamp.range(-2,10)(-11) must_== -2
    }
    "not clamp fitting numbers" in {
      Clamp.range(-2,10)(1) must_== 1
    }
    "work with Long" in {
      Clamp.range(-2L,Long.MaxValue)(-11) must_== -2
    }

    "work with Byte" in {
      val min:Byte = 12
      val max:Byte = 24
      Clamp.range(min,max)(12) must_== 12
    }
    "work with Float" in {
      Clamp.range(.1f,.2f)(.15f) must_== .15f
    }
    "work with Double" in {
      Clamp.range(.1d,.2d)(.15d) must_== .15d
    }
    "work with Char" in {
      Clamp.range('A','Z')('+') must_== 'A'
    }
    "work with Short" in {
      val min:Short = 12
      val max:Short= 24
      Clamp.range(min,max)(13) must_== 13
    }

  }

  "Clamp.lenXXX" should {
    "cut to long Strings (FillLeft)" in {
      Clamp.lenFillLeft("0034",2,'0') must_== "34"
    }
    "cut to long Strings (FillRight)" in {
      Clamp.lenFillRight("ab  ",2,' ') must_== "ab"
    }
    "fill to short Strings (FillLeft)" in {
      Clamp.lenFillLeft("34",4,'0') must_== "0034"
    }
    "fill to short Strings (FillRight)" in {
      Clamp.lenFillRight("ab",4,' ') must_== "ab  "
    }
  }
}
