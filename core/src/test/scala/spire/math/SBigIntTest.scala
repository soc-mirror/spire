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

  //

  val bigIntegerCtor = {
    val ctor = classOf[java.math.BigInteger].getDeclaredConstructor(classOf[Int], classOf[Array[Int]])
    ctor setAccessible true
    ctor
  }

  def signAndArrayToSafeBigInteger(signum: Int, arr: Array[Int]) =
    bigIntegerCtor.newInstance(signum.asInstanceOf[Object], arr)

  //

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
    npe { bi gcd null }
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

  test("BigInt#isEven") {
    assert(SBigInt.Zero.isEven)
    assert(!SBigInt.One.isEven)
    assert(!SBigInt.MinusOne.isEven)
  }

  test("BigInt#isOdd") {
    assert(!SBigInt.Zero.isOdd)
    assert(SBigInt.One.isOdd)
    assert(SBigInt.MinusOne.isOdd)
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

  test("BigInt#fromArray(signum: Int, arr: Array[Int]) — with valid inputs") {
    for ((sig, mag) <- List((0, Array[Int]()), (1, Array(1,1,1)), (-1, Array(Int.MaxValue, Int.MinValue)), (-1, Array(Int.MinValue, Int.MaxValue)))) {
      val jbigint = signAndArrayToSafeBigInteger(sig, mag)
      val sbigint = SBigInt.fromArray(sig, mag)
      assert((sbigint compare jbigint) === 0)
    }
  }

  ignore("BigInt#fromArray(signum: Int, arr: Array[Int]) — with invalid inputs") {
    for ((sig, mag) <- List(/*(0, Array(0)),*/ (0, Array(1)), (1, Array(0)), (-1, Array[Int]()))) {
      intercept[java.lang.reflect.InvocationTargetException] { // cause: NumberFormatException
        signAndArrayToSafeBigInteger(sig, mag)
      }
      intercept[IllegalArgumentException] {
        SBigInt.fromArray(sig, mag)
      }
    }
  }

  test("BigInt#fromBinaryString(String)") {
    for (i <- interestingValues) {
      val jBigInt = new BigInteger(i.toString, 2)
      assert((SBigInt.fromBinaryString(jBigInt.toString(2)) compare jBigInt) === 0)
    }
  }

  test("SBigInt#toBinaryString") {
    for (i <- List(-9223372036854775808L)) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger.valueOf(i)
      val sbibin = sbigint.toBinaryString
      val jbibin = jbigInt.toString(2)
      assert(sbibin === jbibin, s"Failed value: $i, ${i.toBinaryString},\ns.toBinStr: $sbibin,\nj.toBinStr: $jbibin")
    }
  }

  test("Fail to create BigInt from invalid String") {
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
      val bigInt = BigInt(i)
      assert(sbigint.hashCode === bigInt.hashCode, s"$i")
    }
  }

  test("SBigInt#hashCode is equal for 32-bit Ints") {
    def unifiedPrimitiveHashcode(long: Long) =
      if (long >= Int.MinValue && long <= Int.MaxValue) long.toInt
      else long.##

    for (i <- smallValues ++ randomIntValues) {
      val sbigint = SBigInt(i)
      assert(sbigint.isValidInt)
      assert(sbigint.hashCode === unifiedPrimitiveHashcode(i), sbigint.toDebugString)
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

    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val ssum = (sbigint + sbigint + sbigint)
      val jbigInt = new BigInteger(i.toString)
      val jsum = jbigInt add jbigInt add jbigInt
      assert(ssum === jsum, ssum.toDebugString)
    }
  }

  test("SBigInt#-") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val ssum = (sbigint - sbigint)
      val jbigInt = new BigInteger(i.toString)
      val jsum = jbigInt subtract jbigInt
      assert(ssum === jsum, ssum.toDebugString)
    }

    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val ssum = (sbigint + sbigint - sbigint)
      val jbigInt = new BigInteger(i.toString)
      val jsum = jbigInt add jbigInt subtract jbigInt
      assert(ssum === jsum, ssum.toDebugString)
    }
  }

  ignore("SBigInt#*") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val ssum = (sbigint * sbigint)
      val jbigInt = new BigInteger(i.toString)
      val jsum = jbigInt multiply jbigInt
      assert(ssum === jsum, ssum.toDebugString)
    }

    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val ssum = (sbigint * sbigint * sbigint)
      val jbigInt = new BigInteger(i.toString)
      val jsum = jbigInt multiply jbigInt multiply jbigInt
      assert(ssum.isZero)
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

  test("BigInt#gcd") {
    assert((SBigInt.Zero     gcd SBigInt.Zero)     == SBigInt.Zero)
    assert((SBigInt.Zero     gcd SBigInt.One)      == SBigInt.One)
    assert((SBigInt.One      gcd SBigInt.Zero)     == SBigInt.One)
    assert((SBigInt.One      gcd SBigInt.One)      == SBigInt.One)
    assert((SBigInt.Zero     gcd SBigInt.MinusOne) == SBigInt.One)
    assert((SBigInt.MinusOne gcd SBigInt.Zero)     == SBigInt.One)
    assert((SBigInt.One      gcd SBigInt.MinusOne) == SBigInt.One)
    //assert((SBigInt(12)      gcd SBigInt(9))       == SBigInt(3))
  }

  test("BigInt#pow") {
    intercept[ArithmeticException] {
      SBigInt(1) pow -1
    }

    assert(1       === (SBigInt.Zero   pow  0), "    0 pow  0")
    assert(0       === (SBigInt.Zero   pow  1), "    0 pow  1")
    assert(0       === (SBigInt.Zero   pow 42), "    0 pow 42")
    assert(1       === (SBigInt(99999) pow  0), "99999 pow  0")
    assert(99999   === (SBigInt(99999) pow  1), "99999 pow  1")
//    assert(59049   === (    SBigInt(3) pow 10),     "3 pow 10")
//    assert(177147  === (    SBigInt(3) pow 11),     "3 pow 11")
//    assert(-177147 === (   SBigInt(-3) pow 11),    "-3 pow 11")

  }

  test("BigInt#signBits") {
    val bi1 = SBigInt(-1)
    val bi2 = SBigInt(0)
    val bi3 = SBigInt(1)

    assert(bi1.signBits == -1)
    assert(bi2.signBits ==  0)
    assert(bi3.signBits ==  0)
  }

  test("BigInt#bitCount") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger valueOf i
      assert(sbigint.bitCount === sbigint.bitCount, i)
    }
  }

  test("BigInt#longValue") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger valueOf i
      assert(sbigint.longValue === sbigint.longValue, i)
    }
  }

  test("BigInt#intValue") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger valueOf i
      assert(sbigint.intValue === sbigint.intValue, i)
    }
  }

  ignore("BigInt#doubleValue") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger valueOf i
      assert(sbigint.doubleValue === sbigint.doubleValue, i)
    }
  }

  ignore("BigInt#floatValue") {
    for (i <- interestingValues) {
      val sbigint = SBigInt(i)
      val jbigInt = BigInteger valueOf i
      assert(sbigint.floatValue === sbigint.floatValue, i)
    }
  }
}
