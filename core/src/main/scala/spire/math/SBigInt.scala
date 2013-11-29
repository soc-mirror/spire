package spire.math

import language.implicitConversions

import scala.math.{ ScalaNumber, ScalaNumericConversions }
import scala.math.{ BigInt => _ }

import UtilCommon._
import UtilAnnotations._
import UtilBigEndian._

object SBigInt {

  //CONSTANTS∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /**
    * 0 is a bit special: It is the only value which is allowed to have
    * a 0 sign and an empty array as the magnitude.
    *
    * `Zero` is the only instance of value `0`.
    */
  @Finished
  @Tested
  final val Zero: SBigInt = new SBigInt(0, Array[Int]())

  /**
    * `One` is a pre-defined instance of value `1`.
    * Unlike `Zero`, multiple instances with the value of `One` can exist.
    */
  @Finished
  @Tested
  final val One: SBigInt = new SBigInt(1, Array(1))

  final val MinusOne: SBigInt = SBigInt(-1)

  private val LogTwo: Double = math.log(2.0)

  private val StringZeros = Array(
    "", "0", "00", "000", "0000", "00000", "000000", "0000000", "00000000", "000000000",
    "0000000000", "00000000000", "000000000000", "0000000000000", "00000000000000",
    "000000000000000", "0000000000000000", "00000000000000000", "000000000000000000",
    "0000000000000000000", "00000000000000000000", "000000000000000000000",
    "0000000000000000000000", "00000000000000000000000", "000000000000000000000000",
    "0000000000000000000000000", "00000000000000000000000000", "000000000000000000000000000",
    "0000000000000000000000000000", "00000000000000000000000000000",
    "000000000000000000000000000000", "0000000000000000000000000000000",
    "00000000000000000000000000000000", "000000000000000000000000000000000",
    "0000000000000000000000000000000000", "00000000000000000000000000000000000",
    "000000000000000000000000000000000000", "0000000000000000000000000000000000000",
    "00000000000000000000000000000000000000", "000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000", "00000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000", "0000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000000000",
    "00000000000000000000000000000000000000000000000000000000000000",
    "000000000000000000000000000000000000000000000000000000000000000")

  @transient private var logCache: Array[Double] = null

  @transient private var powerCache: Array[java.util.ArrayList[SBigInt]] = null

  //CONSTANTS∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //FACTORIES∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /**
    * Creates an instance with the value of the given signed `Int` value.
    *
    * @param num the signed value from which the BigInt is created
    */
  @Finished
  @Tested
  def apply(num: Int): SBigInt = {
    if (num == 0) return SBigInt.Zero
    // Invariant: `num` not Zero
    var newNum = 0
    var newSign = 0

    if (num < 0) {
      // Make num positive again ...
      newNum = -num
      // and make the sign negative.
      newSign = -1
    } else {
      newNum = num
      newSign = 1
    }

    assume(newSign != 0)
    assume(newNum != 0)

    new SBigInt(newSign, Array(newNum))
  }

  /**
    * Creates an instance with the value of the given signed `Long` value.
    *
    * @param num the signed value from which the BigInt is created
    */
  @Finished
  @Tested
  def apply(num: Long): SBigInt = {
    if (num == 0) return SBigInt.Zero
    // Invariant: `num` not Zero
    var newNum = 0L
    var newSign = 0

    if (num < 0) {
      // Make num positive again ...
      newNum = -num
      // and make the sign negative.
      newSign = -1
    } else {
      newNum = num
      newSign = 1
    }

    assume(newSign != 0)
    assume(newNum != 0)

    // Get the upper 32 bits to figure out how large the array needs to be
    val upperInt: Int = highBitsToInt(newNum)
    if (upperInt != 0)
      // We need an array with 2 elements
      new SBigInt(newSign, Array(upperInt, newNum.toInt))
    else
      // We need an array with 1 element
      new SBigInt(newSign, Array(newNum.toInt))
  }

  /**
    * Creates an instance from a sign `Int` and an `Array[Int]`.
    *
    * The array gets copied.
    *
    * @param signum The sign, either `1`, `0` or `1`.
    * @param arr    A Big Endian array representing an unsigned integer.
    */
  @Endianess(BigEndian)
  def fromArray(signum: Int, arr: Array[Int]) = {
    // Check for `Zero`:
    if (signum == 0) {
      require(arr.length == 0)
      SBigInt.Zero
    } else {
      require(signum == 1 || signum == -1)
      new SBigInt(signum, arr.clone)
    }
  }

  //FACTORIES∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //PARSING∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /**
    * Creates an instance from a decimal String.
    *
    * Examples:
    * `"42"`, `"+0"`, `"-1"`
    *
    * Not supported:
    * `"042"`, `"+-0"`, `"1.0"`, `"0xFF"`
    */
  @Tested
  @WorkInProgress
  @Endianess(LittleEndian)
  @throws[NumberFormatException]
  def apply(str: String): SBigInt = {
    //Ugly but potentially faster than Character.isDigit etc.
    @inline def charToInt(c: Char) =
      if (c >= '0' && c <= '9') c - '0'
      else -1

    val len = str.length
    if (len == 0)
      throw new NumberFormatException("Empty String!")

    var startPos = 0
    var firstChar = str.charAt(0)

    val sign =
      if (firstChar == '+') {
        startPos = 1
        1
      } else if (firstChar == '-') {
        startPos = 1
        -1
      } else
        1

    // Disallow "+" ...
    if (startPos == len)
      throw new NumberFormatException("Missing digits!")

    // Disallow stuff like "042", "+042".
    if (len - 1 > startPos && str.charAt(startPos) == '0')
      throw new NumberFormatException("Octal literal not allowed!")

    /* If there are no more than 9/18 digits (Int/Long.MaxValue.toString.length == 10/19)
     * the String can safely converted to Int/Long.
     */
    if (len - startPos < 9) return apply(str.toInt)
    if (len - startPos < 18) return apply(str.toLong)

    ???
  }

  @throws[NumberFormatException]
  def fromStringOfRadix(s: String, radix: Int) =
    (radix: @annotation.switch) match {
      case 10 => apply(s)
      case 2 => fromBinaryString(s)
      case _ => ???
    }

  @throws[NumberFormatException]
  def fromBinaryString(str: String): SBigInt = {
    @inline def charToInt(c: Char): Int =
      if (c == '0') 0
      else if (c == '1') 1
      else -1

    val len = str.length
    if (len == 0)
      throw new NumberFormatException("Empty String!")

    var startPos = 0
    var firstChar = str.charAt(0)

    val sign =
      if (firstChar == '+') {
        startPos = 1
        1
      } else if (firstChar == '-') {
        startPos = 1
        -1
      } else
        1

    while (startPos < len && str.charAt(startPos) == 0)
      startPos += 1

    if (startPos == len)
      SBigInt.Zero
    else {
      val newLen = (len - startPos)
      var arrSize = {
        var result = newLen / 32
        if (newLen % 32 != 0)
          result += 1
        result
      }

      val digits = new Array[Int](arrSize)

      var current = len-1
      while (current >= startPos) {
        val arrayIndex = current / 32
        val numBitIndex = current % 32
        val numBits = digits(arrayIndex)
        digits(arrayIndex) = numBits | charToInt(str.charAt(current)) << numBitIndex
        current -= 1
      }
      new SBigInt(sign, digits)
    }
  }

  //PARSING∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //HELPERS∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  private lazy val bigIntegerCtor = {
    val ctor = classOf[java.math.BigInteger].getDeclaredConstructor(classOf[Array[Int]], classOf[Int])
    ctor setAccessible true
    ctor
  }

  private def valueOfBigInteger(jbi: java.math.BigInteger): Array[Int] = {
    val valueField = classOf[java.math.BigInteger].getDeclaredField("mag")
    valueField setAccessible true
    (valueField get jbi).asInstanceOf[Array[Int]]
  }

  private def signAndArrayToBigInteger(signum: Int, arr: Array[Int]) =
    bigIntegerCtor.newInstance(arr, signum.asInstanceOf[Object])

  /** Implicit conversion from `Int` to `SBigInt`. */
  implicit def intToBigInt(i: Int): SBigInt = apply(i)

  /** Implicit conversion from `Long` to `SBigInt`. */
  implicit def longToBigInt(l: Long): SBigInt = apply(l)

  /** Implicit conversion from `BigInteger` to `SBigInt`. */
  implicit def bigIntegerToBigInt(jbi: java.math.BigInteger): SBigInt =
    new SBigInt(jbi.signum(), valueOfBigInteger(jbi))

  implicit def bigIntToBigInteger(bi: SBigInt): java.math.BigInteger =
    bigIntegerCtor.newInstance(bi.arr, bi.signum.asInstanceOf[Object])

  //HELPERS∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
}

/**
  * Number class for signed whole numbers with unlimited precision.
  *
  * Internally, the sign is stored as an Int and the magnitude as the two complement in big endian order.
  *
  * It is made sure that the same value has the same underlying representation.
  *
  * {{{
  * index:       0        1        2
  *          ___________________________
  * values:  | ...01 | ...011 | ...010 |  == (1 << 64) + (1 << 33) + (1 << 32) + (1 << 2)
  *          –––––––––––––––––––––––––––
  *            Most-   ....     Least-significant
  * }}}
  */
@ToDo("Check what happens if value requires more storage than an array can provide. (Int.MaxValue)")
@SerialVersionUID(1L)
final class SBigInt private[math] (final val signum: Int, private[math] final val arr: Array[Int])
  extends ScalaNumber with ScalaNumericConversions with Ordered[SBigInt] with Serializable {
  //TODO: Reject magnitudes with bitLength > Int.MaxValue

  //ATTRIBUTESv∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  @Finished
  @Tested
  def isWhole: Boolean = true
  @Finished
  @Tested
  def isZero: Boolean = this eq SBigInt.Zero
  @Finished
  @Tested
  def isOne: Boolean = signum == 1 && arr.length == 1 && arr(0) == 1
  @Finished
  @Tested
  def isEven: Boolean = isZero || ((arr(0) & 0x1) == 0)
  @Finished
  @Tested
  def isOdd: Boolean = !isZero && ((arr(0) & 0x1) == 1)
  @Finished
  @Tested
  def isValidLong: Boolean = this >= Long.MinValue && this <= Long.MaxValue
  /** Returns `true` iff this can be represented exactly by [[scala.Float]]; otherwise returns `false`. */
  def isValidFloat = {
    val bitLen = bitLength
    (bitLen <= 24 ||
      {
        val lowest = lowestSetBit
        bitLen <= java.lang.Float.MAX_EXPONENT + 1 && // exclude this < -2^128 && this >= 2^128
          lowest >= bitLen - 24 &&
          lowest < java.lang.Float.MAX_EXPONENT + 1 // exclude this == -2^128
      })
  }

  /** Returns `true` iff this can be represented exactly by [[scala.Double]]; otherwise returns `false`. */
  def isValidDouble = {
    val bitLen = bitLength
    (bitLen <= 53 ||
      {
        val lowest = lowestSetBit
        bitLen <= java.lang.Double.MAX_EXPONENT + 1 && // exclude this < -2^1024 && this >= 2^1024
          lowest >= bitLen - 53 &&
          lowest < java.lang.Double.MAX_EXPONENT + 1 // exclude this == -2^1024
      })
  }

  @Finished
  @Tested
  def underlying = this

  @inline
  def mag = arr

  def isPositive: Boolean = this.signum == 1
  def isNegative: Boolean = this.signum == -1

  def testBit(atPos: Int): Boolean = ???

  /**
    * Returns the number of bits in the minimal two's-complement representation of this BigInt,
    * excluding a sign bit.
    */
  def bitLength: Int = ???

  /**
    * Returns the number of bits in the two's complement representation of this BigInt
    * that differ from its sign bit.
    */
  def bitCount: Int = {
    var i = 0
    var bits = 0
    val len = arr.length
    while (i < len) {
      bits += Integer bitCount arr(i)
      i += 1
    }
    if (isNegative) {
      // TODO?
    }
    bits
  }

  def lowestSetBit: Int = ???

  //ATRIBUTES∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //CONVERSIONS∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /**
    * Returns a `java.math.BigInteger` of this value.
    * If allowed in the current security context, the underlying array will be shared.
    * Otherwise, the array will be copied. //TODO
    */
  def bigInteger: java.math.BigInteger = SBigInt.signAndArrayToBigInteger(signum, arr)

  /**
    * Returns this value as a `Long`.
    * If the magnitude is too large, the lowest 64 bits will be returned.
    */
  @Endianess(LittleEndian)
  def longValue: Long = {
    if (isZero)
      0
    else {
      var result = arr(0)
      if (arr.length > 1)
        result += arr(1) << 32
      signum * result
    }
  }

  /**
    * Returns this value as an `Int`.
    * If the magnitude is too large, the lowest 32 bits will be returned.
    */
  @Endianess(LittleEndian)
  def intValue: Int =
    if (isZero)
      0
    else
      signum * arr(0)

  /**
    * Returns this value as a `Float`. Might lose precision.
    * If the magnitude is too large, `Float.MaxValue` (iff `sign == 1`)
    * or `Float.MinValue` (iff `sign == -1`) are returned.
    */
  @Finished
  def floatValue: Float = toString.toFloat

  /**
    * Returns this value as a `Double`. Might lose precision.
    * If the magnitude is too large, `Double.MaxValue` (iff `sign == 1`)
    * or `Double.MinValue` (iff `sign == -1`) are returned.
    */
  @Finished
  def doubleValue: Double = toString.toDouble

  //CONVERSIONS∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //ARITHMETIC∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  @Finished
  @Tested
  def unary_- : SBigInt = new SBigInt(-signum, arr)

  @WorkInProgress
  def +(that: SBigInt): SBigInt = {
    /* Check if one of the numbers are zero and return the other one.
     * `that` needs to be checked first, so that a NPE gets thrown if it is null. */
    if (that.isZero) return this
    if (this.isZero) return that

    // Check if both numbers have the same sign.
    // If true, keep the sign and add the numbers.
    if (this.signum == that.signum) {
      val resultArr =
        if (this.arr.length >= that.arr.length)
          arrayPlusArray(this.arr, that.arr)
        else
          arrayPlusArray(that.arr, this.arr)

      return new SBigInt(this.signum, resultArr)
    }

    // Different signs.
    // Compare arrays:
    val result = compareArrays(this.arr, that.arr)
    if (result == 0)
      // Same value, different sign:
      SBigInt.Zero

    val resultArray =
      if (result > 0)
        arrayMinusArray(this.arr, that.arr)
      else
        arrayMinusArray(that.arr, this.arr)

    val resultSignum =
      if (result == this.signum) 1
      else -1

    new SBigInt(resultSignum, resultArray)

  }

  @WorkInProgress
  def -(that: SBigInt): SBigInt = {
    // If that is zero, return this.
    if (that.signum == 0) return this
    // If this is Zero, return that, negated.
    if (this.signum == 0) return -that
    // Invariant: Both numbers are non-zero.

    if (this.signum != that.signum) {
      val resultArr =
        if (this.arr.length >= that.arr.length)
          arrayPlusArray(this.arr, that.arr)
        else
          arrayPlusArray(that.arr, this.arr)

      return new SBigInt(this.signum, resultArr)
    }

    // Different signs.
    // Compare arrays:
    val result = compareArrays(this.arr, that.arr)
    if (result == 0)
      // Same value, different sign:
      SBigInt.Zero

    val resultArray =
      if (result > 0)
        arrayMinusArray(this.arr, that.arr)
      else
        arrayMinusArray(that.arr, this.arr)

    val resultSignum =
      if (result == this.signum) 1
      else -1

    new SBigInt(resultSignum, resultArray)
  }

  def *(that: SBigInt): SBigInt = {
    /* `that` needs to be checked first, so that a NPE gets thrown if it is null. */
    if (that.isZero || this.isZero)
      return SBigInt.Zero

    if (this.isOne)
      return that

    if (that.isOne)
      return this

    val xlen = mag.length;
    val ylen = that.mag.length;

    if ((xlen < KaratsubaThreshold) || (ylen < KaratsubaThreshold)) {
      val resultSign = if (signum == that.signum) 1 else -1
      if (that.mag.length == 1) {
        return multiplyByInt(mag, that.mag(0), resultSign)
      }
      if (mag.length == 1) {
        return multiplyByInt(that.mag, mag(0), resultSign)
      }
      var result: Array[Int] = ??? // multiplyToLen(mag, xlen, that.mag, ylen, null)
      result = removeLeadingZeroes(result)
      return new SBigInt(resultSign, result);
    } else if ((xlen < ToomCookThreshold) && (ylen < ToomCookThreshold))
      return multiplyKaratsuba(this, that);
    else if (!shouldMultiplySchoenhageStrassen(xlen) || !shouldMultiplySchoenhageStrassen(ylen))
      return multiplyToomCook3(this, that);
    else
      return multiplySchoenhageStrassen(this, that);
  }

  def /(that: SBigInt): SBigInt = {
    if (that.isZero)
      throw new ArithmeticException

    if (this.isZero)
      return SBigInt.Zero

    if (that.isOne)
      return this

    if (mag.length < BurnikelZieglerThreshold || that.mag.length < BurnikelZieglerThreshold)
      return divideKnuth(that);
    else if (!shouldDivideBarrett(mag.length * 32) || !shouldDivideBarrett(that.mag.length * 32))
      return divideBurnikelZiegler(that);
    else
      return divideBarrett(that);
  }

  /**
    * Returns a BigInteger whose value is {@code (this / val)} using an O(n^2) algorithm from Knuth.
    *
    * @param val value by which this BigInteger is to be divided.
    * @return {@code this / val}
    * @throws ArithmeticException if {@code val} is zero.
    * @see MutableBigInteger#divideKnuth(MutableBigInteger, MutableBigInteger, boolean)
    */
  private[math] def divideKnuth(that: SBigInt): SBigInt = ???
  private[math] def remainderKnuth(that: SBigInt): SBigInt = ???
  private[math] def divideAndRemainderKnuth(that: SBigInt): (SBigInt, SBigInt) = ???

  /**
    * Estimates whether Barrett Division will be more efficient than Burnikel-Ziegler when
    * dividing two numbers of a given length in bits.
    * @param bitLength the number of bits in each of the two inputs
    * @return <code>true</code> if Barrett is more efficient, <code>false</code> if Burnikel-Ziegler is more efficient
    */
  private def shouldDivideBarrett(bitLength: Int): Boolean = {
    if (bitLength < 3300000)
      return false
    if (bitLength < 4100000)
      return true
    if (bitLength < 5900000)
      return false
    if (bitLength < 8300000)
      return true
    if (bitLength < 9700000)
      return false
    if (bitLength < 16000000)
      return true
    if (bitLength < 19000000)
      return false
    return true
  }

  /**
    * Calculates <code>this / val</code> using the Burnikel-Ziegler algorithm.
    * @param val the divisor
    * @return <code>this / val</code>
    */
  private def divideBurnikelZiegler(that: SBigInt): SBigInt =
    divideAndRemainderBurnikelZiegler(that)._1

  /**
    * Calculates <code>this % val</code> using the Burnikel-Ziegler algorithm.
    * @param val the divisor
    * @return <code>this % val</code>
    */
  private def remainderBurnikelZiegler(that: SBigInt): SBigInt =
    divideAndRemainderBurnikelZiegler(that)._2

  /**
    * Computes <code>this / val</code> and <code>this % val</code> using the
    * Burnikel-Ziegler algorithm.
    * @param val the divisor
    * @return an array containing the quotient and remainder
    */
  private def divideAndRemainderBurnikelZiegler(that: SBigInt): (SBigInt, SBigInt) = {
    var (div, rem) = divideAndRemainderBurnikelZieglerPositive(abs, that.abs)

    // fix signs
    if (signum * that.signum < 0)
      div = -div
    if (signum < 0)
      rem = -rem
    return (div, rem)
  }

  /**
    * Returns a <code>BigInteger</code> containing <code>blockLength</code> ints from
    * <code>this</code> number, starting at <code>index*blockLength</code>.<br/>
    * Used by Burnikel-Ziegler division.
    * @param index the block index
    * @param numBlocks the total number of blocks in <code>this</code> number
    * @param blockLength length of one block in units of 32 bits
    * @return
    */
  private[math] def getBlock(index: Int, numBlocks: Int, blockLength: Int): SBigInt = {
    val blockStart: Int = index * blockLength;
    if (blockStart >= mag.length)
      return SBigInt.Zero

    var blockEnd = 0
    if (index == numBlocks - 1)
      blockEnd = (bitLength() + 31) / 32;
    else
      blockEnd = (index + 1) * blockLength;
    if (blockEnd > mag.length)
      return SBigInt.Zero

    val newMag = removeLeadingZeroes(java.util.Arrays.copyOfRange(mag, mag.length - blockEnd, mag.length - blockStart))
    return new SBigInt(signum, newMag)
  }

  /**
    * Returns a number equal to <code>this.shiftRightInts(n).getLower(n)</code>.<br/>
    * Used by Burnikel-Ziegler division.
    * @param n a non-negative number
    * @return <code>n</code> bits of <code>this</code> starting at bit <code>n</code>
    */
  private[math] def shiftAndTruncate(n: Int): SBigInt = {
    if (mag.length <= n)
      return SBigInt.Zero
    if (mag.length <= 2 * n) {
      val newMag = removeLeadingZeroes(java.util.Arrays.copyOfRange(mag, 0, mag.length - n))
      return new SBigInt(signum, newMag)
    } else {
      val newMag = removeLeadingZeroes(java.util.Arrays.copyOfRange(mag, mag.length - 2 * n, mag.length - n))
      return new SBigInt(signum, newMag)
    }
  }

  /** Barrett division */
  private def divideBarrett(that: SBigInt): SBigInt =
    divideAndRemainderBarrett(that)._1

  /** Barrett division */
  private def remainderBarrett(that: SBigInt): SBigInt =
    divideAndRemainderBarrett(that)._2

  /**
    * Computes <code>this/val</code> and <code>this%val</code> using Barrett division.
    * @param val the divisor
    * @return an array containing the quotient and remainder
    */
  private def divideAndRemainderBarrett(that: SBigInt): (SBigInt, SBigInt) = {
    var (cdiv, crem) = this.abs.divideAndRemainderBarrettPositive(that.abs)
    if (this.signum * that.signum < 0)
      cdiv = -cdiv
    if (signum < 0)
      crem = -crem
    return (cdiv, crem)
  }

  /**
    * Computes <code>this/val</code> and <code>this%val</code> using Barrett division.
    * <code>val</code> must be positive.
    * @param val the divisor
    * @return an array containing the quotient and remainder
    */
  private def divideAndRemainderBarrettPositive(that: SBigInt): (SBigInt, SBigInt) = {
    val m = this.bitLength
    val n = that.bitLength

    if (m < n)
      return (SBigInt.Zero, this)
    else if (m <= 2 * n) {
      // this case is handled by Barrett directly
      val mu = that inverse (m - n)
      return barrettBase(that, mu)
    } else {
      // treat each n-bit piece of a as a digit and do long division by val
      // (which is also n bits), reusing the inverse
      val mu2n = that inverse n
      var startBit: Int = m / n * n; // the bit at which the current n-bit piece starts
      var quotient = SBigInt.Zero
      var remainder = this >> startBit
      val mask = (SBigInt.One << n) - SBigInt.One // n ones
      while (startBit > 0) {
        startBit -= n;
        val ai = (this >> startBit) & mask
        remainder = (remainder << n) + ai
        val mu = mu2n.shiftRightRounded(2 * n - remainder.bitLength) // mu = 2^(remainder.length-n)/val
        val (cdiv, crem) = remainder.barrettBase(that, mu)
        quotient = (quotient << n) + cdiv
        remainder = crem
      }
      return (quotient, remainder)
    }
  }

  /**
    * Computes <code>this/b</code> and <code>this%b</code>.
    * The binary representation of <code>b</code> must be at least half as
    * long, and no longer than, the binary representation of <code>a</code>.<br/>
    * This method uses the Barrett algorithm as described in
    * <a href="http://treskal.com/kalle/exjobb/original-report.pdf">
    * Fast Division of Large Integers</a>, pg 17.
    * @param b
    * @param mu 2<sup>n</sup>/b where <code>n</code> is the number of binary digits of <code>this</code>
    * @return an array containing the quotient and remainder
    */
  private def barrettBase(b: SBigInt, mu: SBigInt): (SBigInt, SBigInt) = {
    val m = bitLength
    val n = b.bitLength

    val a1 = this >> (n - 1)
    var q = (a1 * mu) >> (m - n + 1)
    var r = this - (b * q)
    while (r.signum() < 0 || r.compareTo(b) >= 0)
      if (r.signum() < 0) {
        r = r + b
        q = q - SBigInt.One
      } else {
        r = r - b
        q = q + SBigInt.One
      }
    return (q, r)
  }

  /**
    * Computes 2<sup>bitLength()+n</sup>/this.<br/>
    * Uses the
    * <a href="http://en.wikipedia.org/wiki/Division_%28digital%29#Newton.E2.80.93Raphson_division">
    * Newton algorithm</a> as described in
    * <a href="http://treskal.com/kalle/exjobb/original-report.pdf">
    * Fast Division of Large Integers</a>, pg 23.
    * @param n precision in bits
    * @return <code>1/this</code>, shifted to the left by <code>bitLength()+n</code> bits
    */
  private def inverse(n: Int): SBigInt = {
    val m = bitLength
    if (n <= NewtonThreshold)
      return (SBigInt.One << (n * 2)) divideKnuth (shiftRightRounded(m - n))

    // let numSteps = ceil(log2(n/NEWTON_THRESHOLD)) and initialize k
    var numSteps = bitLengthOf((n + NewtonThreshold - 1) / NewtonThreshold)
    val k = new Array[Int](numSteps)
    var ki = n
    var i = numSteps - 1
    while (i >= 0) {
      ki = (ki + 1) / 2
      k(i) = if (ki < NewtonThreshold) NewtonThreshold else ki
      i -= 1
    }

    // calculate 1/this truncated to k0 fraction digits
    var z = (SBigInt.One << (k(0) * 2)).divideKnuth(shiftRightRounded(m - k(0))) // exp=k0 because exp(this)=m

    var j = 0
    while (j < numSteps) {
      ki = k(j)
      // the following BigIntegers represent numbers of the form a*2^(-exponent)
      val s = z.square // exponent = 2ki
      val t = shiftRightRounded(m - 2 * ki - 3) // exponent = 2ki+3
      val u = t * s // exponent = 4ki+3 > 2ki+1
      var w = z + z // exponent = ki
      w = w << (3 * ki + 3) // increase #fraction digits to 4ki+3 to match u
      z = w - u // exponent = 4ki+3
      if (j < numSteps - 1)
        z = z.shiftRightRounded(4 * ki + 3 - k(j + 1)) // reduce #fraction digits to k[i+1]
      else
        z = z.shiftRightRounded(4 * ki + 3 - n) // final step: reduce #fraction digits to n
      j += 1
    }
    return z;
  }

  /**
    * Same as {@link BigInteger#shiftRight(int)} but rounds to the
    * nearest integer.
    * @param n shift distance, in bits.
    * @return round(this*2<sup>-n</sup>)
    */
  private def shiftRightRounded(n: Int): SBigInt = {
    var b = this >> n
    if (n > 0 && testBit(n - 1))
      b = b + SBigInt.One
    return b;
  }

  /**
    * Shifts a number to the left by a multiple of 32. Used by Burnikel-Ziegler division.
    * @param n a non-negative number
    * @return <code>this.shiftLeft(32*n)</code>
    */
  private[math] def shiftLeftInts(n: Int): SBigInt = {
    val newMag = removeLeadingZeroes(java.util.Arrays.copyOf(mag, mag.length + n))
    new SBigInt(signum, newMag)
  }

  /**
    * Shifts a number to the right by a multiple of 32. Used by Burnikel-Ziegler division.
    * @param n a non-negative number
    * @return <code>this.shiftRight(32*n)</code>
    */
  private[math] def shiftRightInts(n: Int): SBigInt = {
    if (n >= mag.length)
      SBigInt.Zero
    else
      new SBigInt(signum, java.util.Arrays.copyOf(mag, mag.length - n))
  }

  def %(that: SBigInt): SBigInt = ???

  def /%(that: SBigInt): (SBigInt, SBigInt) = {
    if (that == null)
      throw new NullPointerException
    if (mag.length < BurnikelZieglerThreshold || that.mag.length < BurnikelZieglerThreshold)
      return divideAndRemainderKnuth(that)
    else if (!shouldDivideBarrett(mag.length * 32) || !shouldDivideBarrett(that.mag.length * 32))
      return divideAndRemainderBurnikelZiegler(that)
    else
      return divideAndRemainderBarrett(that)
  }

  /**
    * Returns a SBigInt whose value is (this mod m).
    * This method differs from `%` in that it always returns a non-negative SBigInt.
    */
  def mod(that: SBigInt): SBigInt = ???

  /** Returns the greatest common divisor of `this.abs` and `that.abs`. */
  def gcd(that: SBigInt): SBigInt = {
    if (this.isZero)
      that.abs
    else if (that.isZero)
      this.abs
    else if (this.arr.length == 1 && this.arr(0) == 1 || that.arr.length == 1 && that.arr(0) == 1)
      SBigInt.One
    else
      ???
  }

  def square: SBigInt = {
    if (signum == 0)
      return SBigInt.Zero
    val len = mag.length;

    if (len < KaratsubaSquareThreshold) {
      val z = squareToLen(mag, len, null);
      return new SBigInt(1, removeLeadingZeroes(z))
    } else if (len < ToomCookSquareThreshold)
      return squareKaratsuba
    else if (!shouldSquareSchoenhageStrassen(len))
      return squareToomCook3
    else
      return squareSchoenhageStrassen
  }

  private def squareToLen(x: Array[Int], len: Int, z: Array[Int]): Array[Int] = ???
  private def squareKaratsuba = ???
  private def squareToomCook3 = ???
  private def squareSchoenhageStrassen = ???

  /**
    * Estimates whether SS will be more efficient than the other methods when squaring a number
    * of a given length in bits.
    * @param bitLength the number of ints in the number to be squared
    * @return <code>true</code> if SS is more efficient, <code>false</code> if Toom-Cook is more efficient
    * @see #shouldMultiplySchoenhageStrassen(int)
    */
  private def shouldSquareSchoenhageStrassen(length: Int): Boolean = {
    if (IS64BIT) {
      if (length < 15000)
        return false
      if (length < 16384) // 2^14
        return true
      if (length < 27100)
        return false
      if (length < 32768) // 2^15
        return true
      if (length < 43600)
        return false
      if (length < 65536) // 2^16
        return true
      if (length < 76300)
        return false
      if (length < 131072) // 2^17
        return true
      if (length < 133800)
        return false
      return true
    } else {
      if (length < 7100)
        return false
      if (length < 8192) // 2^13
        return true
      if (length < 14200)
        return false
      if (length < 16384) // 2^14
        return true
      if (length < 24100)
        return false
      if (length < 32768) // 2^15
        return true
      if (length < 42800)
        return false
      if (length < 65536) // 2^16
        return true
      if (length < 73000)
        return false
      return true
    }
  }

  /** Returns the minimum of this and that. */
  def min(that: SBigInt): SBigInt =
    if (this <= that) this
    else that

  /** Returns the maximum of this and that. */
  def max(that: SBigInt): SBigInt =
    if (this >= that) this
    else that

  /** Returns a SBigInt whose value is (`this` raised to the power of `exp`). */
  def pow(exponent: Int): SBigInt = {
    if (exponent < 0)
      throw new ArithmeticException(s"Negative exponent: $exponent");

    if (signum == 0) {
      if (exponent == 0)
        return SBigInt.One
      else
        return SBigInt.Zero
    } else {
      if (exponent == 0)
        return SBigInt.One
      if (exponent == 1)
        return this
    }

    var partToSquare = this.abs

    // Factor out powers of two from the base, as the exponentiation of
    // these can be done by left shifts only.
    // The remaining part can then be exponentiated faster. The
    // powers of two will be multiplied back at the end.
    val powersOfTwo = partToSquare.getLowestSetBit();

    var remainingBits = 0

    // Factor the powers of two out quickly by shifting right, if needed.
    if (powersOfTwo > 0) {
      partToSquare = partToSquare >> powersOfTwo
      remainingBits = partToSquare.bitLength
      if (remainingBits == 1) // Nothing left but +/- 1?
        if (signum < 0 && (exponent & 1) == 1)
          return SBigInt.MinusOne << (powersOfTwo * exponent)
        else
          return SBigInt.One << (powersOfTwo * exponent)
    } else {
      remainingBits = partToSquare.bitLength
      if (remainingBits == 1) // Nothing left but +/- 1?
        if (signum < 0 && (exponent & 1) == 1)
          return SBigInt.MinusOne
        else
          return SBigInt.One
    }

    // This is a quick way to approximate the size of the result,
    // similar to doing log2[n] * exponent. This will give an upper bound
    // of how big the result can be, and which algorithm to use.
    val scaleFactor = remainingBits * exponent;

    // Use slightly different algorithms for small and large operands.
    // See if the result will safely fit into a long. (Largest 2^63-1)
    if (partToSquare.mag.length == 1 && scaleFactor <= 62) {
      // Small number algorithm. Everything fits into a long.
      val newSign: Int = if (signum < 0 && (exponent & 1) == 1) -1 else 1
      var result: Long = 1;
      var baseToPow2: Long = partToSquare.mag(0).unsignedToLong

      var workingExponent: Int = exponent;

      // Perform exponentiation using repeated squaring trick
      while (workingExponent != 0) {
        if ((workingExponent & 1) == 1)
          result = result * baseToPow2;

        workingExponent >>>= 1
        if (workingExponent != 0)
          baseToPow2 = baseToPow2 * baseToPow2;
      }

      // Multiply back the powers of two (quickly, by shifting left)
      if (powersOfTwo > 0) {
        val bitsToShift: Int = powersOfTwo * exponent;
        if (bitsToShift + scaleFactor <= 62) // Fits in long?
          return SBigInt((result << bitsToShift) * newSign)
        else
          return SBigInt(result * newSign).shiftLeft(bitsToShift)
      } else
        return SBigInt(result * newSign)
    } else {
      // Large number algorithm. This is basically identical to
      // the algorithm above, but calls multiply() and square()
      // which may use more efficient algorithms for large numbers.
      var answer: SBigInt = SBigInt.One;

      var workingExponent: Int = exponent;
      // Perform exponentiation using repeated squaring trick
      while (workingExponent != 0) {
        if ((workingExponent & 1) == 1)
          answer = answer.multiply(partToSquare);

        workingExponent >>>= 1
        if (workingExponent != 0)
          partToSquare = partToSquare.square
      }
      // Multiply back the (exponentiated) powers of two (quickly,
      // by shifting left)
      if (powersOfTwo > 0)
        answer = answer.shiftLeft(powersOfTwo * exponent);

      if (signum < 0 && (exponent & 1) == 1)
        return -answer
      else
        return answer
    }
  }

  @Tested
  def abs = if (signum < 0) -this else this

  //ARITHMETIC∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //BIT-OPERATIONS∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /** Leftshift of SBigInt. */
  def <<(n: Int): SBigInt = {
    // Handle special cases
    if (isZero) return SBigInt.Zero
    if (n == 0) return this
    if (n < 0)
      if (n == Int.MinValue)
        throw new UnsupportedOperationException("Int.MinValue")
      else
        return >>(-n)

    // Common case
    val resArr = shiftLeft(arr, n)

    new SBigInt(signum, resArr)
  }

  /** (Signed) rightshift of SBigInt. */
  def >>(n: Int): SBigInt = {
    // Handle special cases
    if (isZero) return SBigInt.Zero
    if (n == 0) return this
    if (n < 0) return <<(-n);

    // Common case
    ???
  }

  @ToDo
  def unary_~ : SBigInt = {
    val result = new Array[Int](arr.length)
    var i = 0
    while (i < arr.length) {
      result(i) = ~arr(i)
      i += 1
    }
    new SBigInt(???, result)
  }

  /** Bitwise and of SBigInts. */
  def &(that: SBigInt): SBigInt = ???

  /** Bitwise or of SBigInts. */
  def |(that: SBigInt): SBigInt = ???

  /** Bitwise exclusive-or of SBigInts. */
  def ^(that: SBigInt): SBigInt = ???

  /** Bitwise and-not of SBigInts. Returns a SBigInt whose value is (this & ~that). */
  def &~(that: SBigInt): SBigInt = ???

  // We don't use Integral, right? What to do here?
  //def until(end: SBigInt, step: SBigInt = SBigInt(1)) = NumericRange(this, end, step)
  //def to(end: SBigInt, step: SBigInt = SBigInt(1)) = NumericRange.inclusive(this, end, step)

  //BIT-OPERATIONS∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //EQUALITY∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /** Compares this SBigInt with the specified value for equality. */
  override def equals(that: Any): Boolean = that match {
    case that: SBigInt => this equalsSBigInt that
    case that: java.math.BigInteger => this equalsBigInteger that
    case that: BigDecimal => that.toBigIntExact exists (this equals _)
    //    case that: Double     => isValidDouble && toDouble == that
    //    case that: Float      => isValidFloat && toFloat == that
    case x => isValidLong && unifiedPrimitiveEquals(x)
  }

  private def equalsBigInteger(that: java.math.BigInteger): Boolean =
    this.bigInteger == that //Is there a better way? FIXME

  private def equalsSBigInt(that: SBigInt): Boolean =
    (this eq that) || sameArrayContents(this.arr, that.arr)

  override def hashCode: Int =
    if (isValidLong)
      unifiedPrimitiveHashcode
    else {
      var hash = 0

      val len = arr.length
      var i = 0
      while (i < len) {
        hash = (31 * hash + (arr(i) & UnsignedIntMask)).toInt
        i += 1
      }

      hash * signum
    }

  final def compare(that: SBigInt): Int = {
    if (this.signum == that.signum) {
      (this.signum: @annotation.switch) match {
        case 1 => compareArrays(this.arr, that.arr)
        case -1 => compareArrays(that.arr, this.arr)
        case 0 => 0
      }
    } else {
      if (this.signum > that.signum) 1
      else -1
    }
  }

  //EQUALITY∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //PRINTING∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  override def toString: String = toString(10)

  def toString(radix: Int): String = {
    if (radix < Character.MIN_RADIX)
      throw new UnsupportedOperationException(s"Radix $radix is too small. Minimum radix is ${Character.MIN_RADIX}.")
    if (radix > Character.MAX_RADIX)
      throw new UnsupportedOperationException(s"Radix $radix is too large. Maximum radix is ${Character.MAX_RADIX}.")

    if (signum == 0)
      return "0";

    // If it's small enough, use smallToString.
    if (mag.length <= SchönhageBaseConversionThreshold)
      return smallToString(radix)

    // Otherwise use recursive toString, which requires positive arguments.
    // The results will be concatenated into this StringBuilder
    val sb = new java.lang.StringBuilder
    if (signum < 0) {
      toString(-this, sb, radix, 0)
      sb.insert(0, '-');
    } else
      toString(this, sb, radix, 0)

    return sb.toString()
  }

  /** This method is used to perform toString when arguments are small. */
  private def smallToString(radix: Int): String = {
    if (signum == 0)
      return "0"
    ???
  }

  /**
    * Converts the specified BigInteger to a string and appends to
    * <code>sb</code>. This implements the recursive Schoenhage algorithm
    * for base conversions.
    * <p/>
    * See Knuth, Donald, _The Art of Computer Programming_, Vol. 2,
    * Answers to Exercises (4.4) Question 14.
    *
    * @param u The number to convert to a string.
    * @param sb The StringBuilder that will be appended to in place.
    * @param radix The base to convert to.
    * @param digits The minimum number of digits to pad to.
    */
  private def toString(u: SBigInt, sb: java.lang.StringBuilder, radix: Int, digits: Int): Unit = {
    /* If we're smaller than a certain threshold, use the smallToString
     * method, padding with leading zeroes when necessary. */
    if (u.mag.length <= SchönhageBaseConversionThreshold) {
      val s = u smallToString radix

      // Pad with internal zeros if necessary.
      // Don't pad if we're at the beginning of the string.
      if ((s.length < digits) && (sb.length > 0)) {
        var i = s.length
        while (i < digits) {
          sb.append('0') // do this?
          i += 1
        }
      }

      sb.append(s);
      return ;
    }

    val b = u.bitLength();

    // Calculate a value for n in the equation radix^(2^n) = u
    // and subtract 1 from that value. This is used to find the
    // cache index that contains the best value to divide u.
    val n: Int = math.round(math.log(b * SBigInt.LogTwo / (SBigInt logCache radix)) / SBigInt.LogTwo - 1.0).toInt
    val v = radixConversionCache(radix, n);
    val results = u.divideAndRemainder(v)

    val expectedDigits = 1 << n;

    // Now recursively build the two halves of each number.
    toString(results(0), sb, radix, digits - expectedDigits)
    toString(results(1), sb, radix, expectedDigits)
  }

  /**
    * Returns the value radix^(2^exponent) from the cache.
    * If this value doesn't already exist in the cache, it is added.
    * <p/>
    * This could be changed to a more complicated caching method using
    * <code>Future</code>.
    */
  private def radixConversionCache(radix: Int, exponent: Int): SBigInt = {
    this.synchronized {
      var retVal: SBigInt = null;
      val cacheLine = SBigInt.powerCache(radix)
      val oldSize = cacheLine.size
      if (exponent >= oldSize) {
        cacheLine.ensureCapacity(exponent + 1);
        var i = oldSize
        while (i <= exponent) {
          retVal = cacheLine.get(i - 1).square
          cacheLine.add(i, retVal);
          i += 1
        }
      } else
        retVal = cacheLine.get(exponent);

      return retVal;
    }
  }

  def toBinaryString: String = signToString + binStr

  private def binStr: String = {
    val buf = new StringBuilder
    var i = 0
    while (i < arr.length) {
      paddedBinaryString(buf, arr(i))
      i += 1
    }
    if (buf.isEmpty) "0"
    else buf.toString
  }

  private def paddedBinaryString(sb: StringBuilder, i: Int): Unit = {
    val str = i.toBinaryString
    val missingChars = 32 - str.length
    if (missingChars > 0) {
      sb ++= ("0" * missingChars)
    }
    sb ++= str
  }

  def toDebugString: String = signToString + binDebStr

  private def binDebStr = {
    val buf = new StringBuilder
    var i = 0
    while (i < arr.length) {
      val str = arr(i).toBinaryString
      val zeroes = "0" * (32 - str.length)
      buf ++= i + ":" ++= zeroes ++= str += '|'
      i += 1
    }
    buf.toString
  }

  private def signToString = if (signum == -1) "-" else ""

  //PRINTING∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //ACCESS∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//
  /**
    * Iff the value is negative return 0xFFFFFFFF (all bits set),
    * else 0x00000000 (no bits set).
    */
  @Finished
  @Tested
  final def signBits: Int =
    if (signum < 0) -1
    else 0

  /**
    * Returns values of the arr array, with special behaviour if the requested
    * index is greater or less than the magnitude.
    *
    * TODO: Better name.
    */
  private def apply(pos: Int): Int =
    // Return 0 if i is negative. (Or throw an exception?)
    if (pos < 0)
      0
    else
      ???

  /**
    * Returns a new BigInteger representing n lower ints of the number.
    * This is used by Karatsuba multiplication, Karatsuba squaring,
    * and Burnikel-Ziegler division.
    */
  final def lowerInts(n: Int): SBigInt = {
    val len = mag.length;

    if (len <= n)
      return this

    val lowerInts = new Array[Int](n)
    scala.compat.Platform.arraycopy(mag, len - n, lowerInts, 0, n);

    return new SBigInt(1, removeLeadingZeroes(lowerInts))
  }

  /**
    * Returns a new BigInteger representing mag.length-n upper
    * ints of the number. This is used by Karatsuba multiplication,
    * Karatsuba squaring, and Burnikel-Ziegler division.
    */
  final def upperInts(n: Int): SBigInt = {
    val len = mag.length;

    if (len <= n)
      return SBigInt.Zero

    val upperLen = len - n;
    val upperInts = new Array[Int](upperLen)
    scala.compat.Platform.arraycopy(mag, 0, upperInts, 0, upperLen);

    return new SBigInt(1, removeLeadingZeroes(upperInts));
  }

  //ACCESS∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//
  //SERIALIZATION∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨//

  /**
    * Standard Serialization would work, but we have to make sure
    * that we sanitize the array to verify our invariant of no leading
    * “zeroes” in our magnitude.
    *
    * Otherwise all methods depending on it will be broken.
    */
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.lang.ClassNotFoundException])
  private def readObject(in: java.io.ObjectInputStream): Unit = {
    @inline def setField(name: String, value: Any): Unit = {
      val field = this.getClass.getDeclaredField(name)
      field.setAccessible(true)
      field.set(this, value)
    }

    var inArr = in.readObject.asInstanceOf[Array[Int]]
    setField("arr", removeLeadingZeroes(inArr))

    if (inArr.length != 0) {
      val sign = if (in.readBoolean) 1 else -1
      setField("signum", sign)
    }
  }

  @throws(classOf[java.io.ObjectStreamException])
  private def readReplace(): Object = {
    if (signum == 0) return SBigInt.Zero
    else this
  }

  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeObject(arr)
    if (signum != 0)
      out.writeBoolean(signum > 0)
    out.close()
  }

  //SERIALIZATION∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧//

  /**
    * Returns a slice of a BigInteger for use in Toom-Cook multiplication.
    * @param lowerSize The size of the lower-order bit slices.
    * @param upperSize The size of the higher-order bit slices.
    * @param slice The index of which slice is requested, which must be a
    * number from 0 to size-1. Slice 0 is the highest-order bits,
    * and slice size-1 are the lowest-order bits.
    * Slice 0 may be of different size than the other slices.
    * @param fullsize The size of the larger integer array, used to align
    * slices to the appropriate position when multiplying different-sized
    * numbers.
    */
  def toomSlice(lowerSize: Int, upperSize: Int, slice: Int, fullsize: Int): SBigInt = {
    //int start, end, sliceSize, len, offset;

    val len = mag.length;
    val offset = fullsize - len;
    var start = 0
    var end = 0

    if (slice == 0) {
      start = 0 - offset;
      end = upperSize - 1 - offset;
    } else {
      start = upperSize + (slice - 1) * lowerSize - offset;
      end = start + lowerSize - 1;
    }

    if (start < 0)
      start = 0;
    if (end < 0)
      return SBigInt.Zero;

    val sliceSize = (end - start) + 1;

    if (sliceSize <= 0)
      return SBigInt.Zero;

    // While performing Toom-Cook, all slices are positive and
    // the sign is adjusted when the final number is composed.
    if (start == 0 && sliceSize >= len)
      return this.abs

    val intSlice: Array[Int] = new Array[Int](sliceSize)
    scala.compat.Platform.arraycopy(mag, start, intSlice, 0, sliceSize)

    return new SBigInt(1, removeLeadingZeroes(intSlice))
  }

  /**
    * Does an exact division (that is, the remainder is known to be zero)
    * of the specified number by 3. This is used in Toom-Cook
    * multiplication. This is an efficient algorithm that runs in linear
    * time. If the argument is not exactly divisible by 3, results are
    * undefined. Note that this is expected to be called with positive
    * arguments only.
    */
  private[math] def exactDivideBy3: SBigInt = {
    val len: Int = mag.length;
    var result: Array[Int] = new Array[Int](len)

    var x: Long = 0L
    var w: Long = 0L
    var q: Long = 0L
    var borrow: Long = 0L

    var i = len - 1

    while (i >= 0) {

      x = mag(i).unsignedToLong
      w = x - borrow;
      if (borrow > x) // Did we make the number go negative?
        borrow = 1L;
      else
        borrow = 0L;

      // 0xAAAAAAAB is the modular inverse of 3 (mod 2^32). Thus,
      // the effect of this is to divide by 3 (mod 2^32).
      // This is much faster than division on most architectures.
      q = (w * 0xAAAAAAABL) & UnsignedIntMask
      result(i) = q.toInt

      // Now check the borrow. The second check can of course be
      // eliminated if the first fails.
      if (q >= 0x55555556L) {
        borrow += 1
        if (q >= 0xAAAAAAABL)
          borrow += 1;
      }
      i -= 1
    }
    result = removeLeadingZeroes(result)
    return new SBigInt(signum, result)
  }
}
