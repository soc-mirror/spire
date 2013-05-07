package spire.math

import org.scalatest.FunSuite
import spire.implicits.{ eqOps ⇒ _, _ }

import UtilCommon._

class UtilCommonTest extends FunSuite {
  test("unsignedIntToLong") {
    assert(Int.MinValue.unsignedToLong === -Int.MinValue.toLong)
    assert(-2.unsignedToLong === 2* Int.MaxValue.toLong)
    assert(-1.unsignedToLong === 2* Int.MaxValue.toLong + 1L)
    assert(0.unsignedToLong === 0)
    assert(1.unsignedToLong === 1L)
    assert(Int.MaxValue.unsignedToLong === Int.MaxValue.toLong)
  }

  test("longToUnsignedInt") {
    assert(Int.MinValue === (-Int.MinValue.toLong).toUnsignedInt)
    assert(-2 === (2* Int.MaxValue.toLong).toUnsignedInt)
    assert(-1 === (2* Int.MaxValue.toLong + 1).toUnsignedInt)
    assert(0.toUnsignedInt === 0)
    assert(1.toUnsignedInt === 1)
    assert(Int.MaxValue.toLong.toUnsignedInt === Int.MaxValue)
  }
}
