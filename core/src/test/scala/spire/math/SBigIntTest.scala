package spire.math

import org.scalatest.FunSuite
import spire.implicits.{eqOps => _, _}
import java.math.BigInteger

class SBigIntTest extends FunSuite {

  val smallValues = (-100L to 100L)
  val intMinMaxValues =
    (Int.MaxValue.toLong - 100 to Int.MaxValue.toLong + 100) ++
    (Int.MinValue.toLong - 100 to Int.MinValue.toLong + 100)
  val longMinMaxValues =
    (Long.MaxValue - 100 to Long.MaxValue) ++
    (Long.MinValue to Long.MinValue + 100)
  val randomIntValues = 1 to 100 map {_ => scala.util.Random.nextInt.toLong}

  val interestingValues =
    smallValues ++
    intMinMaxValues ++
    longMinMaxValues ++
    randomIntValues

  test("BigInt#arr should not be accessible") {
    intercept[NoSuchFieldException] {
      classOf[SBigInt].getField("arr")
    }
  }

  test("BigInt's methods should throw NPE's for nulls") {
    val bi = SBigInt.Zero
    @inline def npe(f: => Any) = intercept[NullPointerException](f)

    npe { bi + null }
    npe { bi - null }
    npe { bi * null }
    npe { bi / null }
//    npe { bi % null }
    npe { bi /% null }
//    npe { bi mod null }
//    npe { bi gcd null }
    npe { bi min null }
    npe { bi max null }
//    npe { bi & null }
//    npe { bi | null }
//    npe { bi ^ null }
//    npe { bi &~ null }
  }

  test("BigInt(0) == null should be false") {
    assert(SBigInt.Zero != null)
  }

  test("BigInt(0) same as BigInt.Zero") {
    assert(SBigInt(0) eq SBigInt.Zero)
  }

  test("BigInt#isZero") {
    assert(SBigInt.Zero.isZero)
    assert(!SBigInt.One.isZero)
  }

  test("BigInt#isOne") {
    assert(!SBigInt.Zero.isOne)
    assert(SBigInt.One.isOne)
  }

  test("BigInt(0) equal to BigInt.Zero") {
    assert(SBigInt(0) === SBigInt.Zero)
  }

  test("BigInt(1) equal to BigInt.One") {
    assert(SBigInt(1) === SBigInt.One)
  }

  test("BigInt#isWhole") {
    assert(SBigInt.Zero.isWhole)
  }

  test("BigInt#underlying") {
    val bi = SBigInt(42)
    assert(SBigInt.Zero eq SBigInt.Zero.underlying)
    assert(SBigInt.One eq SBigInt.One.underlying)
    assert(bi eq bi.underlying)
  }

  test("BigInt#isValidLong") {
    val allValid = interestingValues.forall(i => SBigInt(i).isValidLong)
    assert(allValid === true)
  }

  test("BigInt#apply(Int(42))") {
    val bi = SBigInt(42)
    assert(bi.signum === +1)
    assert(bi.arr === Array(42))
  }

 test("BigInt#compare(BigInt)") {
    val bi = SBigInt(42)
    val bi2 = SBigInt(43)
    assert(bi < bi2 === true)
    assert(bi <= bi2 === true)
    assert(bi > bi2 === false)
    assert(bi >= bi2 === false)
  }

  ignore("Equality of toStrings") {
    for (i <- interestingValues) {
      assert(SBigInt(i).toString === BigInteger.valueOf(i).toString)
    }
  }

  test("BigInt#bigInteger") {
    for (i <- interestingValues) {
      assert(SBigInt(i).bigInteger === new BigInteger(i.toString))
    }
  }

  ignore("BigInt#apply(String)") {
    for (i <- interestingValues) {
      assert(SBigInt(i.toString).toString === new BigInteger(i.toString).toString, i)
    }
  }

  ignore("BigInt#fromBinaryString(String)") {
    for (i <- interestingValues) {
      val jBigInt = new BigInteger(i.toString, 2)
      assert(SBigInt.fromBinaryString(jBigInt.toString(2)) === jBigInt)
    }
  }

  ignore("Fail to create BigInt from invalid String") {
    val invalidStrings = Array("042", "+", "+-1", "-+1", "1.0", ".5", "1.", "0xFF", "1-1", "1+1")
    for (i <- invalidStrings) {
      intercept[NumberFormatException]{
        SBigInt(i)
      }
    }
  }

  test("Serialization") {
    import java.io._

    for (i <- interestingValues) {

      val bi = SBigInt(i)

      val buffer = new ByteArrayOutputStream
      val out = new ObjectOutputStream(buffer)
      out writeObject bi

      out.close()

      val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
      val bi2 = in.readObject
      in.close()

      assert(bi === bi2)
    }
  }

  test("SBigInt#toBinaryString") {
    for (i <- List(-9223372036854775808L)) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger.valueOf(i)
      assert(sbigint.toBinaryString === jbigInt.toString(2), s"Failed value: $i,\ns.toBinStr: ${i.toBinaryString},\nj.toBinStr: ${jbigInt.toString(2)}")
    }
  }

  test("SBigInt#unary_-") {
    for (i <- interestingValues) {
      val sbigint = -SBigInt(i)
      val jbigInt = new BigInteger(i.toString).negate
      assert((sbigint compare jbigInt) == 0)
    }
  }

  ignore("SBigInt#hashCode is equal to BigInt") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val oldbigInt = BigInt(i.toString)
      assert(sbigint.hashCode === oldbigInt.hashCode, interestingValues indexOf i)
    }
  }

  ignore("SBigInt#hashCode is equal for 32-bit Ints") {
    def unifiedPrimitiveHashcode(long: Long) =
      if (long >= Int.MinValue && long <= Int.MaxValue) long.toInt
      else long.##

    for (i <- smallValues ++ randomIntValues) {
      val sbigint = SBigInt(i)
      assert(sbigint.isValidInt)
      assert(sbigint.hashCode === unifiedPrimitiveHashcode(i), sbigint.toString)
    }
  }

  test("SBigInt#+") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val ssum = (sbigint + sbigint)
      val jbigInt = new BigInteger(i.toString)
      val jsum = jbigInt add jbigInt
      assert(ssum === jsum, ssum.toDebugString)
    }
  }

  test("BigInt#abs") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val jbigInt = new BigInteger(i.toString)
      assert(sbigint.abs === sbigint.abs)
    }
  }

  test("BigInt#signBits") {
    val bi1 = SBigInt(-1)
    val bi2 = SBigInt(0)
    val bi3 = SBigInt(1)

    assert(bi1.signBits == -1)
    assert(bi2.signBits == 0)
    assert(bi3.signBits == 0)
  }
}
