package spire.math

import UtilCommon._
import UtilAnnotations._

object UtilBigEndian {

  /** Returns the original array if unchanged. */
  final def removeLeadingZeroes(arr: Array[Int]): Array[Int] = {
    var i = 0
    var empty = -1
    var stop = false
    while (i < arr.length && stop == false) {
      if (arr(i) == 0)
        empty = i
      else
        stop = true
      i += 1
    }
    if (empty == -1) {
      arr
    } else {
      val newLen = arr.length - empty - 1
      val newArr = new Array[Int](newLen)
      scala.compat.Platform.arraycopy(arr, empty + 1, newArr, 0, newLen)
      newArr
    }
  }

  /** Returns a new array. */
  final def removeLeadingZeroesAndCopy(arr: Array[Int]): Array[Int] = {
    var i = 0
    var empty = -1
    var stop = false
    while (i < arr.length && stop == false) {
      if (arr(i) == 0)
        empty = i
      else
        stop = true
      i += 1
    }
    if (empty == -1) {
      arr.clone
    } else {
      val newLen = arr.length - empty - 1
      val newArr = new Array[Int](newLen)
      scala.compat.Platform.arraycopy(arr, empty + 1, newArr, 0, newLen)
      newArr
    }
  }

  final def compareArrays(an: Array[Int], bn: Array[Int]): Int = {
    val al = an.length
    val bl = bn.length
    if (al < bl)
      return -1
    if (al > bl)
      return 1

    var i = 0
    while (i < al) {
      val av = an(i)
      val bv = bn(i)
      if (av != bv)
        return if (av.unsignedToLong < bv.unsignedToLong) -1 else 1
      i += 1
    }

    return 0
  }

  final def inplaceAdd(a: Array[Int], aSize: Int, addend: Int) = {
    var carry: Long = addend.unsignedToLong

    var i = a.length - 1
    val aStop = a.length - aSize // ???
    while ((carry != 0) && (i >= aStop)) { // ???
      carry += a(i).unsignedToLong
      a(i) = carry.toInt
      carry >>= 32

      i += 1
    }

    carry.toInt
  }

  /** Callers of this method have to make sure that the size of `large` is equal or greater than `small`. */
  final def arrayPlusArray(large: Array[Int], small: Array[Int]): Array[Int] = {
    val llen = large.length
    val slen = small.length
    val diff = llen - slen
    /* We could probably check under the condition that b < a if b[high] is "smaller"
      * than Int.MinValue (unsigned) because then b.length would be enough under all
      * circumstances for the new array.
      * Because it doesn't overflow that often, we just assume that it doesn't
      * overflow and reallocate only if it really does later.
      */
    var result = new Array[Int](llen)
    var sum = 0L

    var i = slen - 1
    while (i >= 0) {
      val idx = diff + i
      sum = large(idx).unsignedToLong + small(i).unsignedToLong + sum
      result(idx) = sum.toInt
      sum = sum >>> 32
      i -= 1
    }

    var j = diff - 1
    while (sum != 0 && j >= 0) {
      sum = large(j).unsignedToLong + sum
      result(j) = sum.toInt
      sum = sum >>> 32
      j -= 1
    }

    if (sum == 0 && j >= 0) { // sum == 0 necessary?
      scala.compat.Platform.arraycopy(large, 0, result, 0, j + 1)
      result
    } else if (sum != 0) {
      val oldResult = result
      result = new Array[Int](llen + 1)
      scala.compat.Platform.arraycopy(oldResult, 0, result, 1, oldResult.length)
      result(0) = 1
      result
    } else
      result
  }

  /** Callers of this method have to make sure that the size of `large` is equal or greater than `small`. */
  final def arrayMinusArray(large: Array[Int], small: Array[Int]): Array[Int] = {
    val llen = large.length
    val slen = small.length

    var lidx = llen
    var sidx = slen

    val result = new Array[Int](lidx)

    var diff: Long = 0
    while (sidx > 0) {
      lidx -= 1
      sidx -= 1
      diff = large(lidx).unsignedToLong - small(sidx).unsignedToLong + (diff >> 32)
      result(lidx) = diff.toInt
    }

    var borrow: Boolean = (diff >> 32 != 0)
    while (lidx > 0 && borrow) {
      lidx -= 1
      val tmp = large(lidx) - 1
      result(lidx) = tmp
      borrow = tmp == -1
    }

    while (lidx > 0) {
      lidx -= 1
      result(lidx) = large(lidx)
    }

    removeLeadingZeroes(result)
  }

  final def multiplyByInt(x: Array[Int], y: Int, sign: Int): SBigInt = ???

  final def multiplyToLength(x: Array[Int], xlen: Int, y: Array[Int], ylen: Int, _z: Array[Int]) = ???

  /**
    * Multiplies two BigIntegers using the Karatsuba multiplication
    * algorithm. This is a recursive divide-and-conquer algorithm which is
    * more efficient for large numbers than what is commonly called the
    * "grade-school" algorithm used in multiplyToLen. If the numbers to be
    * multiplied have length n, the "grade-school" algorithm has an
    * asymptotic complexity of O(n^2). In contrast, the Karatsuba algorithm
    * has complexity of O(n^(log2(3))), or O(n^1.585). It achieves this
    * increased performance by doing 3 multiplies instead of 4 when
    * evaluating the product. As it has some overhead, should be used when
    * both numbers are larger than a certain threshold (found
    * experimentally).
    *
    * See: http://en.wikipedia.org/wiki/Karatsuba_algorithm
    */
  final def multiplyKaratsuba(x: SBigInt, y: SBigInt): SBigInt = {
    val xlen: Int = x.arr.length
    val ylen: Int = y.arr.length

    // The number of ints in each half of the number.
    val half: Int = (math.max(xlen, ylen) + 1) / 2

    // xl and yl are the lower halves of x and y respectively, xh and yh are the upper halves.
    val xl = x lowerInts half
    val xh = x upperInts half
    val yl = y lowerInts half
    val yh = y upperInts half

    val p1 = xh * yh
    val p2 = xl * yl

    val p3 = (xh + xl) * (yh + yl)

    // result = p1 * 2^(32*2*half) + (p3 - p1 - p2) * 2^(32*half) + p2
    val result = p1.<<(32 * half).+(p3.-(p1).-(p2)).<<(32 * half).+(p2);

    if (x.signum != y.signum)
      return -result
    else
      return result
  }

  /**
    * Multiplies two BigIntegers using a 3-way Toom-Cook multiplication
    * algorithm. This is a recursive divide-and-conquer algorithm which is
    * more efficient for large numbers than what is commonly called the
    * "grade-school" algorithm used in multiplyToLen. If the numbers to be
    * multiplied have length n, the "grade-school" algorithm has an
    * asymptotic complexity of O(n^2). In contrast, 3-way Toom-Cook has a
    * complexity of about O(n^1.465). It achieves this increased asymptotic
    * performance by breaking each number into three parts and by doing 5
    * multiplies instead of 9 when evaluating the product. Due to overhead
    * (additions, shifts, and one division) in the Toom-Cook algorithm, it
    * should only be used when both numbers are larger than a certain
    * threshold (found experimentally). This threshold is generally larger
    * than that for Karatsuba multiplication, so this algorithm is generally
    * only used when numbers become significantly larger.
    *
    * The algorithm used is the "optimal" 3-way Toom-Cook algorithm outlined
    * by Marco Bodrato.
    *
    * See: http://bodrato.it/toom-cook/
    * http://bodrato.it/papers/#WAIFI2007
    *
    * "Towards Optimal Toom-Cook Multiplication for Univariate and
    * Multivariate Polynomials in Characteristic 2 and 0." by Marco BODRATO;
    * In C.Carlet and B.Sunar, Eds., "WAIFI'07 proceedings", p. 116-133,
    * LNCS #4547. Springer, Madrid, Spain, June 21-22, 2007.
    *
    */
  final def multiplyToomCook3(a: SBigInt, b: SBigInt): SBigInt = {
    val alen = a.arr.length
    val blen = b.arr.length

    val largest: Int = math.max(alen, blen)

    // k is the size (in ints) of the lower-order slices.
    val k: Int = (largest + 2) / 3; // Equal to ceil(largest/3)

    // r is the size (in ints) of the highest-order slice.
    val r: Int = largest - 2 * k;

    // Obtain slices of the numbers. a2 and b2 are the most significant
    // bits of the numbers a and b, and a0 and b0 the least significant.
    val a2 = a.toomSlice(k, r, 0, largest)
    val a1 = a.toomSlice(k, r, 1, largest)
    val a0 = a.toomSlice(k, r, 2, largest)
    val b2 = b.toomSlice(k, r, 0, largest)
    val b1 = b.toomSlice(k, r, 1, largest)
    val b0 = b.toomSlice(k, r, 2, largest)

    val v0 = a0 * b0
    var da1 = a2 + a0
    var db1 = b2 + b0
    val vm1 = (da1 - a1) * (db1 - b1)
    da1 += a1
    db1 += b1
    val v1 = da1 * db1
    val v2 = da1.+(a2).<<(1).-(a0).*(db1.+(b2).<<(1).-(b0))
    val vinf = a2 * b2

    /* The algorithm requires two divisions by 2 and one by 3.
     * All divisions are known to be exact, that is, they do not produce
     * remainders, and all results are positive. The divisions by 2 are
     * implemented as right shifts which are relatively efficient, leaving
     * only an exact division by 3, which is done by a specialized
     * linear-time algorithm. */
    var t2 = (v2 - vm1).exactDivideBy3
    var tm1 = (v1 - vm1) >> 1
    var t1 = v1 - v0
    t2 = (t2 - t1) >> 1
    t1 = t1 - tm1 - vinf
    t2 -= (vinf << 1)
    tm1 -= t2

    // Number of bits to shift left.
    val ss: Int = k * 32;

    val result: SBigInt = vinf.<<(ss).+(t2).<<(ss).+(t1).<<(ss).+(tm1).<<(ss).+(v0)

    if (a.signum != b.signum)
      -result
    else
      result
  }

  /**
    * Multiplies two {@link BigInteger}s using the
    * <a href="http://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm">
    * Schoenhage-Strassen algorithm</a> algorithm.
    * @param a
    * @param b
    * @return a <code>BigInteger</code> equal to <code>a.multiply(b)</code>
    */
  final def multiplySchoenhageStrassen(_a: SBigInt, _b: SBigInt): SBigInt = {
    var a = _a
    var b = _b

    // remove any minus signs, multiply, then fix sign
    val signum = a.signum * b.signum
    if (a.signum() < 0)
      a = -a
    if (b.signum() < 0)
      b = -b

    val cArr = multiplySchoenhageStrassen(a.mag, a.bitLength(), b.mag, b.bitLength());

    var c = SBigInt.fromArray(1, cArr);
    if (signum < 0)
      c = -c

    return c;
  }

  /**
    * This is the core Schoenhage-Strassen method. It multiplies two <b>positive</b> numbers of length
    * <code>aBitLen</code> and </code>bBitLen</code> that are represented as int arrays, i.e. in base
    * 2<sup>32</sup>.
    * Positive means an int is always interpreted as an unsigned number, regardless of the sign bit.<br/>
    * The arrays must be ordered most significant to least significant, so the most significant digit
    * must be at index 0.
    * <p/>
    * The Schoenhage-Strassen algorithm algorithm works as follows:
    * <ol>
    * <li>Given numbers a and b, split both numbers into pieces of length 2<sup>n-1</sup> bits.</li>
    * <li>Take the low n+2 bits of each piece of a, zero-pad them to 3n+5 bits,
    * and concatenate them to a new number u.</li>
    * <li>Do the same for b to obtain v.</li>
    * <li>Calculate all pieces of z' by multiplying u and v (using Schoenhage-Strassen or another
    * algorithm). The product will contain all pieces of a*b mod n+2.</li>
    * <li>Pad the pieces of a and b from step 1 to 2<sup>n+1</sup> bits.</li>
    * <li>Perform a
    * <a href="http://en.wikipedia.org/wiki/Discrete_Fourier_transform_%28general%29#Number-theoretic_transform">
    * Discrete Fourier Transform</a> (DFT) on the padded pieces.</li>
    * <li>Calculate all pieces of z" by multiplying the i-th piece of a by the i-th piece of b.</li>
    * <li>Perform an Inverse Discrete Fourier Transform (IDFT) on z". z" will contain all pieces of
    * a*b mod Fn where Fn=2<sup>2<sup>n+1</sup></sup>.</li>
    * <li>Calculate all pieces of z such that each piece is congruent to z' modulo n+2 and congruent to
    * z" modulo Fn. This is done using the
    * <a href="http://en.wikipedia.org/wiki/Chinese_remainder_theorem">Chinese remainder theorem</a>.</li>
    * <li>Calculate c by adding z<sub>i</sub> * 2<sup>i*2<sup>n-1</sup></sup> for all i, where z<sub>i</sub> is the
    * i-th piece of z.</li>
    * <li>Return c reduced modulo 2<sup>2<sup>m+1</sup></sup>.</li>
    * </ol>
    *
    * References:
    * <ol>
    * <li><a href="http://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm">
    * Wikipedia article</a>
    * <li><a href="http://www.scribd.com/doc/68857222/Schnelle-Multiplikation-gro%C3%9Fer-Zahlen">
    * Arnold Schoenhage und Volker Strassen: Schnelle Multiplikation grosser Zahlen, Computing 7, 1971,
    * Springer-Verlag, S. 281-292</a></li>
    * <li><a href="http://malte-leip.net/beschreibung_ssa.pdf">Eine verstaendliche Beschreibung des
    * Schoenhage-Strassen-Algorithmus</a></li>
    * </ol>
    * @param a
    * @param aBitLen
    * @param b
    * @param bBitLen
    * @return a*b
    */
  final def multiplySchoenhageStrassen(a: Array[Int], aBitLen: Int, b: Array[Int], bBitLen: Int): Array[Int] = {
    // set M to the number of binary digits in a or b, whichever is greater
    val M: Int = math.max(aBitLen, bBitLen)

    // find the lowest m such that m>=log2(2M)
    val m: Int = 32 - Integer.numberOfLeadingZeros(2 * M - 1 - 1)

    val n: Int = m / 2 + 1

    // split a and b into pieces 1<<(n-1) bits long; assume n>=6 so pieces start and end at int boundaries
    val even = m % 2 == 0
    val numPieces: Int = if (even) 1 << n else 1 << (n + 1)
    val pieceSize: Int = 1 << (n - 1 - 5) // in ints

    // build u and v from a and b, allocating 3n+5 bits in u and v per n+2 bits from a and b, resp.
    val numPiecesA = (a.length + pieceSize) / pieceSize
    val u = new Array[Int]((numPiecesA * (3 * n + 5) + 31) / 32)
    var uBitLength = 0
    var i = 0
    while (i < numPiecesA && i * pieceSize < a.length) {
      appendBits(u, uBitLength, a, i * pieceSize, n + 2)
      uBitLength += 3 * n + 5;
      i += 1
    }
    val numPiecesB: Int = (b.length + pieceSize) / pieceSize;
    val v = new Array[Int]((numPiecesB * (3 * n + 5) + 31) / 32)
    var vBitLength = 0
    var j = 0
    while (j < numPiecesB && j * pieceSize < b.length) {
      appendBits(v, vBitLength, b, j * pieceSize, n + 2)
      vBitLength += 3 * n + 5
      j += 1
    }

    val gamma: Array[Int] = (SBigInt.fromArray(1, u) * (SBigInt.fromArray(1, v))).arr // gamma = u * v
    val gammai: Array[Array[Int]] = splitBits(gamma, 3 * n + 5);
    val halfNumPcs: Int = numPieces / 2

    val zi: Array[Array[Int]] = Array.ofDim[Int](gammai.length, 0)

    var k = 0
    while (k < gammai.length) {
      zi(k) = gammai(k)
      k += 1
    }

    var l = 0
    while (l < gammai.length - halfNumPcs) {
      subModPow2(zi(l), gammai(l + halfNumPcs), n + 2)
      l += 1
    }

    var o = 0
    while (o < gammai.length - 2 * halfNumPcs) {
      addModPow2(zi(o), gammai(o + 2 * halfNumPcs), n + 2)
      o += 1
    }

    var p = 0
    while (p < gammai.length - 3 * halfNumPcs) {
      subModPow2(zi(p), gammai(p + 3 * halfNumPcs), n + 2)
      p += 1
    }

    // zr mod Fn
    val ai: Array[Array[Int]] = splitInts(a, halfNumPcs, pieceSize, 1 << (n + 1 - 5))
    val bi: Array[Array[Int]] = splitInts(b, halfNumPcs, pieceSize, 1 << (n + 1 - 5))
    dft(ai, m, n)
    dft(bi, m, n)
    modFn(ai)
    modFn(bi)
    val c: Array[Array[Int]] = Array.ofDim[Int](halfNumPcs, 0)

    var q = 0
    while (q < c.length) {
      c(q) = multModFn(ai(q), bi(q))
      q += 1
    }

    idft(c, m, n)
    modFn(c)

    val z: Array[Int] = new Array[Int](1 << (m + 1 - 5))
    // calculate zr mod Fm from zr mod Fn and zr mod 2^(n+2), then add to z

    var r: Int = 0
    while (r < halfNumPcs) {
      val eta: Array[Int] = if (r >= zi.length) new Array[Int]((n + 2 + 31) / 32) else zi(r)

      // zi = delta = (zi-c[i]) % 2^(n+2)
      subModPow2(eta, c(r), n + 2)

      // z += zr<<shift = [ci + delta*(2^2^n+1)] << [i*2^(n-1)]
      val shift = r * (1 << (n - 1 - 5)) // assume n>=6
      addShifted(z, c(r), shift)
      addShifted(z, eta, shift)
      addShifted(z, eta, shift + (1 << (n - 5)))
      r += 1
    }

    modFn(z); // assume m>=5
    return z
  }

  /**
    * Adds two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>numBits</sup>.
    * Both input values are given as <code>int</code> arrays.
    * The result is returned in the first argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit
    * @param b a number in base 2<sup>32</sup> starting with the highest digit
    */
  final def addModPow2(a: Array[Int], b: Array[Int], numBits: Int): Unit = {
    val numElements = (numBits + 31) / 32
    var carry = false;

    var aIdx = a.length - 1
    var bIdx = b.length - 1
    var i: Int = numElements - 1
    while (i >= 0) {
      var sum = a(aIdx) + b(bIdx)
      if (carry)
        sum += 1
      carry = ((sum >>> 31) < (a(aIdx) >>> 31) + (b(bIdx) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
      a(aIdx) = sum
      aIdx -= 1
      bIdx -= 1
      i -= 1
    }
    a(0) &= -1 >>> (32 - (numBits % 32))
    var j: Int = a.length - 1 - numElements
    while (j >= 0) {
      a(j) = 0
      j -= 1
    }
  }

  /**
    * Subtracts two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>numBits</sup>.
    * Both input values are given as <code>int</code> arrays.
    * The result is returned in the first argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit
    * @param b a number in base 2<sup>32</sup> starting with the highest digit
    */
  final def subModPow2(a: Array[Int], b: Array[Int], numBits: Int): Unit = {
    val numElements = (numBits + 31) / 32;
    var carry = false;
    var aIdx = a.length - 1
    var bIdx = b.length - 1
    var i = numElements - 1
    while (i >= 0) {
      var diff = a(aIdx) - b(bIdx);
      if (carry)
        diff -= 1
      carry = ((diff >>> 31) > (a(aIdx) >>> 31) - (b(bIdx) >>> 31)); // carry if signBit(diff) > signBit(a)-signBit(b)
      a(aIdx) = diff;
      aIdx -= 1
      bIdx -= 1
      i -= 1
    }
    a(aIdx + 1) &= -1 >>> (32 - (numBits % 32))
    while (aIdx >= 0) {
      a(aIdx) = 0
      aIdx -= 1
    }
  }

  /**
    * Performs a
    * <a href="http://en.wikipedia.org/wiki/Discrete_Fourier_transform_%28general%29#Number-theoretic_transform">
    * Fermat Number Transform</a> on an array whose elements are <code>int</code> arrays.<br/>
    * <code>A</code> is assumed to be the lower half of the full array and the upper half is assumed to be all zeros.
    * The number of subarrays in <code>A</code> must be 2<sup>n</sup> if m is even and 2<sup>n+1</sup> if m is odd.<br/>
    * Each subarray must be ceil(2<sup>n-1</sup>) bits in length.<br/>
    * n must be equal to m/2-1.
    * @param A
    * @param m
    * @param n
    */
  final def dft(A: Array[Array[Int]], m: Int, n: Int): Unit = {
    val even = m % 2 == 0;
    val len = A.length;
    var v = 1;
    val d = new Array[Int](A(0).length)

    var slen = len / 2 // slen = #consecutive coefficients for which the sign (add/sub) and x are constant
    while (slen > 0) {

      var j = 0
      while (j < len) {
        var idx = j;
        val x = dftExponent(n, v, idx + len, even);

        var k = slen - 1
        while (k >= 0) {
          cyclicShiftLeftBits(A(idx + slen), x, d);
          scala.compat.Platform.arraycopy(A(idx), 0, A(idx + slen), 0, A(idx).length); // copy A[idx] into A[idx+slen]
          addModFn(A(idx), d);
          subModFn(A(idx + slen), d);
          idx += 1
          k -= 1
        }

        j += 2 * slen
      }

      v += 1
      slen /= 2
    }
  }

  /**
    * Returns the power to which to raise omega in a DFT.<br/>
    * Omega itself is either 2 or 4 depending on m, but when omega=4 this method
    * doubles the exponent so omega can be assumed always to be 2 in a DFT.
    * @param n
    * @param v
    * @param idx
    * @param even
    * @return
    */
  final def dftExponent(n: Int, v: Int, idx: Int, even: Boolean): Int = {
    // take bits n-v..n-1 of idx, reverse them, shift left by n-v-1
    var x = Integer.reverse(idx) << (n - v) >>> (31 - n);

    // if m is even, divide by two
    if (even)
      x >>>= 1

    x
  }

  /**
    * Performs a modified
    * <a href="http://en.wikipedia.org/wiki/Discrete_Fourier_transform_%28general%29#Number-theoretic_transform">
    * Inverse Fermat Number Transform</a> on an array whose elements are <code>int</code> arrays.
    * The modification is that the last step (the one where the upper half is subtracted from the lower half)
    * is omitted.<br/>
    * <code>A</code> is assumed to be the upper half of the full array and the upper half is assumed to be all zeros.
    * The number of subarrays in <code>A</code> must be 2<sup>n</sup> if m is even and 2<sup>n+1</sup> if m is odd.<br/>
    * Each subarray must be ceil(2<sup>n-1</sup>) bits in length.<br/>
    * n must be equal to m/2-1.
    * @param A
    * @param m
    * @param n
    */
  final def idft(A: Array[Array[Int]], m: Int, n: Int): Unit = {
    val even = m % 2 == 0;
    val len = A.length;
    var v = n - 1;
    val c = new Array[Int](A(0).length)

    var slen = 1 // slen = #consecutive coefficients for which the sign (add/sub) and x are constant
    while (slen <= len / 2) {
      var j = 0
      while (j < len) {
        var idx = j
        var idx2 = idx + slen // idx2 is always idx+slen
        val x = idftExponent(n, v, idx, even)

        var k = slen - 1
        while (k >= 0) {
          scala.compat.Platform.arraycopy(A(idx), 0, c, 0, c.length) // copy A[idx] into c
          addModFn(A(idx), A(idx2))
          cyclicShiftRight(A(idx), 1, A(idx))

          subModFn(c, A(idx2))
          cyclicShiftRight(c, x, A(idx2))
          idx += 1
          idx2 += 1
          k -= 1
        }
        j += 2 * slen
      }

      v -= 1
      slen *= 2
    }
  }

  /**
    * Returns the power to which to raise omega in an IDFT.<br/>
    * Omega itself is either 2 or 4 depending on m, but when omega=4 this method
    * doubles the exponent so omega can be assumed always to be 2 in a IDFT.
    * @param n
    * @param v
    * @param idx
    * @param even
    * @return
    */
  final def idftExponent(n: Int, v: Int, idx: Int, even: Boolean): Int = {
    var x = Integer.reverse(idx) << (n - v) >>> (32 - n)
    x += (if (even) 1 << (n - v) else 1 << (n - 1 - v))
    return x + 1
  }

  /**
    * Adds two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>2<sup>n+1</sup></sup>,
    * where n is <code>a.length*32/2</code>; in other words, n is half the number of bits in
    * <code>a</code>.<br/>
    * Both input values are given as <code>int</code> arrays; they must be the same length.
    * The result is returned in the first argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param b a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    */
  final def addModFn(a: Array[Int], b: Array[Int]): Unit = {
    var carry = false
    var i = a.length - 1
    while (i >= 0) {
      var sum = a(i) + b(i)
      if (carry)
        sum += 1
      carry = ((sum >>> 31) < (a(i) >>> 31) + (b(i) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
      a(i) = sum
      i -= 1
    }

    // take a mod Fn by adding any remaining carry bit to the lowest bit;
    // since Fn is congruent to 1 (mod 2^n), it suffices to add 1
    var j = a.length - 1
    while (carry) {
      var sum = a(j) + 1
      a(j) = sum;
      carry = sum == 0
      j -= 1
      if (j < 0)
        j = a.length
    }
  }

  /**
    * Subtracts two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo 2<sup>2<sup>n+1</sup></sup>,
    * where n is <code>a.length*32/2</code>; in other words, n is half the number of bits in
    * <code>a</code>.<br/>
    * Both input values are given as <code>int</code> arrays; they must be the same length.
    * The result is returned in the first argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param b a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    */
  final def subModFn(a: Array[Int], b: Array[Int]): Unit = {
    // subtraction works by shifting b by b.length/2, then adding a and b
    var carry = false;
    var bIdx: Int = b.length / 2 - 1
    var i = a.length - 1
    while (i >= a.length / 2) {
      var sum = a(i) + b(bIdx)
      if (carry)
        sum += 1
      carry = ((sum >>> 31) < (a(i) >>> 31) + (b(bIdx) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
      a(i) = sum;
      bIdx -= 1
      i -= 1
    }
    bIdx = b.length - 1
    var j = a.length / 2 - 1
    while (i >= 0) {
      var sum = a(i) + b(bIdx)
      if (carry)
        sum += 1
      carry = ((sum >>> 31) < (a(i) >>> 31) + (b(bIdx) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
      a(i) = sum
      bIdx -= 1
      i -= 1
    }

    // take a mod Fn by adding any remaining carry bit to the lowest bit;
    // since Fn is congruent to 1 (mod 2^n), it suffices to add 1
    var k = a.length - 1;
    while (carry) {
      var sum = a(k) + 1;
      a(k) = sum
      carry = sum == 0;
      k -= 1
      if (k < 0)
        k = a.length;
    }
  }

  /**
    * Multiplies two <b>positive</b> numbers (meaning they are interpreted as unsigned) modulo Fn
    * where Fn=2<sup>2<sup>n+1</sup></sup>, and returns the result in a new array.<br/>
    * <code>a</code> and <code>b</code> are assumed to be reduced mod Fn, i.e. 0&le;a&lt;Fn and 0&le;b&lt;Fn,
    * where n is <code>a.length*32/2</code>; in other words, n is half the number of bits in
    * <code>a</code>.<br/>
    * Both input values are given as <code>int</code> arrays; they must be the same length.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param b a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    */
  final def multModFn(a: Array[Int], b: Array[Int]): Array[Int] = {
    val a0 = java.util.Arrays.copyOfRange(a, a.length / 2, a.length)
    val b0 = java.util.Arrays.copyOfRange(b, b.length / 2, b.length)

    val aBigInt = SBigInt.fromArray(1, a0)
    val bBigInt = SBigInt.fromArray(1, b0)
    val c = (aBigInt * bBigInt).arr

    // make sure c is the same length as a and b
    val cpad = new Array[Int](a.length)
    scala.compat.Platform.arraycopy(c, 0, cpad, a.length - c.length, c.length)

    val n = a.length / 2;
    // special case: if a=Fn-1, add b*2^2^n which is the same as subtracting b
    if (a(n - 1) == 1) {
      val b0pad = new Array[Int](cpad.length)
      scala.compat.Platform.arraycopy(b0, 0, b0pad, cpad.length - b0.length, b0.length)
      subModFn(cpad, b0pad)
    }
    if (b(n - 1) == 1) {
      val a0pad = new Array[Int](cpad.length)
      scala.compat.Platform.arraycopy(a0, 0, a0pad, cpad.length - a0.length, a0.length)
      subModFn(cpad, a0pad)
    }
    return cpad
  }

  final def modFn(a: Array[Int]): Unit = {
    val len = a.length
    var carry = false

    var i = len - 1
    while (i >= len / 2) {
      val bi = a(i - len / 2)
      var diff = a(i) - bi
      if (carry)
        diff -= 1
      carry = ((diff >>> 31) > (a(i) >>> 31) - (bi >>> 31)); // carry if signBit(diff) > signBit(a)-signBit(b)
      a(i) = diff;
      i -= 1
    }

    var j = len / 2 - 1
    while (j >= 0) {
      a(j) = 0
      j -= 1
    }

    // if result is negative, add Fn; since Fn is congruent to 1 (mod 2^n), it suffices to add 1
    if (carry) {
      var k = len - 1;
      do {
        var sum = a(j) + 1;
        a(k) = sum;
        carry = sum == 0;
        k -= 1
        if (k <= 0)
          k = len;
      } while (carry);
    }
  }

  /**
    * Reduces all subarrays modulo 2<sup>2<sup>n+1</sup></sup> where n=<code>a[i].length*32/2</code> for all i;
    * in other words, n is half the number of bits in the subarray.
    * @param a int arrays whose length is a power of 2
    */
  final def modFn(a: Array[Array[Int]]): Unit = {
    var i = 0
    while (i < a.length) {
      modFn(a(i))
      i += 1
    }
  }

  /**
    * Cyclicly shifts a number to the right modulo 2<sup>2<sup>n+1</sup></sup>, where n is
    * <code>a.length*32/2</code>; in other words, n is half the number of bits in <code>a</code>.<br/>
    * "Right" means towards the lower array indices and the lower bits; this is equivalent to
    * a multiplication by 2<sup>-numBits</sup> modulo 2<sup>2<sup>n+1</sup></sup>.<br/>
    * The result is returned in the third argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param numBits the shift amount in bits
    * @param b the return value; must be at least as long as <code>a</code>
    */
  final def cyclicShiftRight(a: Array[Int], _numBits: Int, b: Array[Int]): Unit = {
    var numBits = _numBits
    val numElements = numBits / 32;
    scala.compat.Platform.arraycopy(a, 0, b, numElements, a.length - numElements);
    scala.compat.Platform.arraycopy(a, a.length - numElements, b, 0, numElements);

    numBits = numBits % 32;
    if (numBits != 0) {
      val bhi = b(b.length - 1);
      b(b.length - 1) = b(b.length - 1) >>> numBits

      var i = b.length - 1
      while (i > 0) {
        b(i) |= b(i - 1) << (32 - numBits)
        b(i - 1) = b(i - 1) >>> numBits
        i -= 1
      }
      b(0) |= bhi << (32 - numBits)
    }
  }

  /**
    * Cyclicly shifts a number to the left modulo 2<sup>2<sup>n+1</sup></sup>, where n is
    * <code>a.length*32/2</code>; in other words, n is half the number of bits in <code>a</code>.<br/>
    * "Left" means towards the lower array indices and the lower bits; this is equivalent to
    * a multiplication by 2<sup>numBits</sup> modulo 2<sup>2<sup>n+1</sup></sup>.<br/>
    * The result is returned in the third argument.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit; the length must be a power of 2
    * @param numBits the shift amount in bits
    * @param b the return value; must be at least as long as <code>a</code>
    */
  final def cyclicShiftLeftBits(a: Array[Int], _numBits: Int, b: Array[Int]): Unit = {
    var numBits = _numBits
    val numElements = numBits / 32;
    scala.compat.Platform.arraycopy(a, numElements, b, 0, a.length - numElements);
    scala.compat.Platform.arraycopy(a, 0, b, a.length - numElements, numElements);

    numBits = numBits % 32;
    if (numBits != 0) {
      val b0 = b(0)
      b(0) <<= numBits

      var i = 1
      while (i < b.length) {
        b(i - 1) |= b(i) >>> (32 - numBits);
        b(i) <<= numBits
        i += 1
      }
      b(b.length - 1) |= b0 >>> (32 - numBits);
    }
  }

  /**
    * Adds two numbers, <code>a</code> and <code>b</code>, after shifting <code>b</code> by
    * <code>numElements</code> elements.<br/>
    * Both numbers are given as <code>int</code> arrays and must be <b>positive</b> numbers
    * (meaning they are interpreted as unsigned).</br> The result is returned in the first
    * argument.
    * If any elements of b are shifted outside the valid range for <code>a</code>, they are dropped.
    * @param a a number in base 2<sup>32</sup> starting with the highest digit
    * @param b a number in base 2<sup>32</sup> starting with the highest digit
    * @param numElements
    */
  final def addShifted(a: Array[Int], b: Array[Int], numElements: Int): Unit = {
    var carry = false;
    var aIdx = a.length - 1 - numElements
    var bIdx = b.length - 1
    var i = math.min(aIdx, bIdx)
    while (i >= 0) {
      val ai = a(aIdx)
      var sum = ai + b(bIdx)
      if (carry)
        sum += 1
      carry = ((sum >>> 31) < (ai >>> 31) + (b(bIdx) >>> 31)) // carry if signBit(sum) < signBit(a)+signBit(b)
      a(aIdx) = sum
      i -= 1
      aIdx -= 1
      bIdx -= 1
    }
    while (carry) {
      a(aIdx) += 1
      carry = a(aIdx) == 0;
      aIdx -= 1
    }
  }

  /**
    * Reads <code>bBitLength</code> bits from <code>b</code>, starting at array index
    * <code>bStart</code>, and copies them into <code>a</code>, starting at bit
    * <code>aBitLength</code>. The result is returned in <code>a</code>.
    * @param a
    * @param aBitLength
    * @param b
    * @param bStart
    * @param bBitLength
    */
  final def appendBits(a: Array[Int], aBitLength: Int, b: Array[Int], bStart: Int, bBitLength: Int): Unit = {
    var aIdx: Int = a.length - 1 - aBitLength / 32
    val bit32: Int = aBitLength % 32

    var i = bStart + bBitLength / 32 - 1
    while (i >= bStart) {
      if (bit32 > 0) {
        a(aIdx) |= b(i) << bit32
        aIdx -= 1
        a(aIdx) = b(i) >>> (32 - bit32)
      } else {
        a(aIdx) = b(i)
        aIdx -= 1
      }
      i -= 1
    }

    if (bBitLength % 32 > 0) {
      aIdx = a.length - 1 - (aBitLength / 32 + bBitLength / 32)
      val bIdx = bBitLength / 32
      var bi = b(b.length - 1 - bStart + bIdx)
      bi &= -1 >>> (32 - bBitLength)
      a(aIdx) |= bi << bit32
      if (bit32 + (bBitLength % 32) > 32)
        a(aIdx - 1) = bi >>> (32 - bit32)
    }
  }

  /**
    * Divides an <code>int</code> array into pieces <code>bitLength</code> bits long.
    * @param a
    * @param bitLength
    * @return a new array containing <code>bitLength</code> bits from <code>a</code> in each subarray
    */
  final def splitBits(a: Array[Int], bitLength: Int): Array[Array[Int]] = {
    var aIntIdx: Int = a.length - 1
    var aBitIdx: Int = 0
    val numPieces: Int = (a.length * 32 + bitLength - 1) / bitLength
    val pieceLength: Int = (bitLength + 31) / 32 // in ints
    val b: Array[Array[Int]] = Array.ofDim[Int](numPieces, pieceLength)

    var i = 0
    while (i < b.length) {
      var bitsRemaining: Int = math.min(bitLength, a.length * 32 - i * bitLength);
      var bIntIdx: Int = bitLength / 32;
      if (bitLength % 32 == 0)
        bIntIdx -= 1
      var bBitIdx = 0;
      while (bitsRemaining > 0) {
        var bitsToCopy: Int = Math.min(32 - aBitIdx, 32 - bBitIdx);
        bitsToCopy = math.min(bitsRemaining, bitsToCopy);
        var mask: Int = a(aIntIdx) >>> aBitIdx;
        mask &= -1 >>> (32 - bitsToCopy);
        mask <<= bBitIdx;
        b(i)(bIntIdx) |= mask;
        bitsRemaining -= bitsToCopy;
        aBitIdx += bitsToCopy;
        if (aBitIdx >= 32) {
          aBitIdx -= 32;
          aIntIdx -= 1
        }
        bBitIdx += bitsToCopy;
        if (bBitIdx >= 32) {
          bBitIdx -= 32;
          bIntIdx -= 1
        }
      }
      i += 1
    }
    return b;
  }

  /**
    * Splits an <code>int</code> array into pieces of <code>pieceSize ints</code> each, and
    * pads each piece to <code>targetPieceSize ints</code>.
    * @param a the input array
    * @param numPieces the number of pieces to split the array into
    * @param pieceSize the size of each piece in the input array in <code>ints</code>
    * @param targetPieceSize the size of each piece in the output array in <code>ints</code>
    * @return an array of length <code>numPieces</code> containing subarrays of length <code>targetPieceSize</code>
    */
  final def splitInts(a: Array[Int], numPieces: Int, pieceSize: Int, targetPieceSize: Int): Array[Array[Int]] = {
    val ai: Array[Array[Int]] = Array.ofDim[Int](numPieces, targetPieceSize)
    var i: Int = 0
    while (i < a.length / pieceSize) {
      scala.compat.Platform.arraycopy(a, a.length - i * pieceSize - pieceSize, ai(i), targetPieceSize - pieceSize, pieceSize)
      i += 1
    }
    scala.compat.Platform.arraycopy(a, a.length - a.length / pieceSize * pieceSize - (a.length % pieceSize), ai(a.length / pieceSize), targetPieceSize - (a.length % pieceSize), a.length % pieceSize)
    return ai
  }

  /**
    * Computes <code>a/b</code> and <code>a%b</code> using the
    * <a href="http://cr.yp.to/bib/1998/burnikel.ps"> Burnikel-Ziegler algorithm</a>.
    * This method implements algorithm 3 from pg. 9 of the Burnikel-Ziegler paper.
    * The parameter beta is 2<sup>32</sup> so all shifts are multiples of 32 bits.<br/>
    * <code>a</code> and <code>b</code> must be nonnegative.
    * @param a the dividend
    * @param b the divisor
    * @return an array containing the quotient and remainder
    */
  final def divideAndRemainderBurnikelZieglerPositive(_a: SBigInt, _b: SBigInt): (SBigInt, SBigInt) = {
    var a = _a
    var b = _b
    val r: Int = a.mag.length
    val s: Int = b.mag.length

    if (r < s)
      return (SBigInt.Zero, a)
    else {
      // let m = min{2^k | (2^k)*BURNIKEL_ZIEGLER_THRESHOLD > s}
      val m: Int = 1 << (32 - Integer.numberOfLeadingZeros(s / BurnikelZieglerThreshold))

      val j: Int = (s + m - 1) / m // j = ceil(s/m)
      val n: Int = j * m // block length in 32-bit units
      val n32: Int = 32 * n // block length in bits
      val sigma: Int = math.max(0, n32 - b.bitLength)
      b = b << sigma // shift b so its length is a multiple of n
      a = a << sigma // shift a by the same amount

      // t is the number of blocks needed to accommodate 'a' plus one additional bit
      var t: Int = (a.bitLength() + n32) / n32
      if (t < 2)
        t = 2
      val a1 = a getBlock (t - 1, t, n) // the most significant block of a
      val a2 = a getBlock (t - 2, t, n) // the second to most significant block

      // do schoolbook division on blocks, dividing 2-block numbers by 1-block numbers
      var z = (a1 shiftLeftInts n) + a2 // Z[t-2]
      var quotient = SBigInt.Zero
      var c: (SBigInt, SBigInt) = null
      var i = t - 2
      while (i > 0) {
        c = divide2n1n(z, b)
        z = a getBlock (i - 1, t, n)
        z = z + (c._2 shiftLeftInts n)
        quotient = (quotient + c._1) shiftLeftInts n
        i -= 1
      }
      // do the loop one more time for i=0 but leave z unchanged
      c = divide2n1n(z, b)
      quotient = quotient + c._1

      val remainder = c._2 >> sigma // a and b were shifted, so shift back
      return (quotient, remainder)
    }
  }

  /**
    * This method implements algorithm 1 from pg. 4 of the Burnikel-Ziegler paper.
    * It divides a 2n-digit number by a n-digit number.<br/>
    * The parameter beta is 2<sup>32</sup> so all shifts are multiples of 32 bits.
    * @param a a nonnegative number such that <code>a.bitLength() <= 2*b.bitLength()</code>
    * @param b a positive number such that <code>b.bitLength()</code> is even
    * @return <code>a/b</code> and <code>a%b</code>
    */
  final def divide2n1n(a: SBigInt, b: SBigInt): (SBigInt, SBigInt) = {
    val n = b.mag.length;
    if (n % 2 != 0 || n < BurnikelZieglerThreshold)
      return a divideAndRemainderKnuth b

    // view a as [a1,a2,a3,a4] and divide [a1,a2,a3] by b
    val (c1div, c1rem) = divide3n2n(a shiftRightInts (n / 2), b)

    // divide the concatenation of c1[1] and a4 by b
    val a4 = a lowerInts (n / 2)
    val (c2div, c2rem) = divide3n2n((c1rem shiftLeftInts (n / 2)) + a4, b)

    // quotient = the concatentation of the two above quotients
    ((c1div shiftLeftInts (n / 2)) + c2div, c2rem)
  }

  /**
    * This method implements algorithm 2 from pg. 5 of the Burnikel-Ziegler paper.
    * It divides a 3n-digit number by a 2n-digit number.<br/>
    * The parameter beta is 2<sup>32</sup> so all shifts are multiples of 32 bits.<br/>
    * @param a a nonnegative number such that <code>2*a.bitLength() <= 3*b.bitLength()</code>
    * @param b a positive number such that <code>b.bitLength()</code> is even
    * @return <code>a/b</code> and <code>a%b</code>
    */
  final def divide3n2n(a: SBigInt, b: SBigInt): (SBigInt, SBigInt) = {
    val n: Int = b.mag.length / 2; // half the length of b in ints

    // split a in 3 parts of length n or less
    val a1 = a shiftRightInts (2 * n)
    val a2 = a shiftAndTruncate n
    val a3 = a lowerInts n

    // split a in 2 parts of length n or less
    val b1 = b shiftRightInts n
    val b2 = b lowerInts n

    var q: SBigInt = null
    var r1: SBigInt = null
    val a12 = (a1 shiftLeftInts n) + a2 // concatenation of a1 and a2
    if (a1.compareTo(b1) < 0) {
      // q=a12/b1, r=a12%b1
      val (div, rem) = divide2n1n(a12, b1)
      q = div
      r1 = rem
    } else {
      // q=beta^n-1, r=a12-b1*2^n+b1
      q = ones(n)
      r1 = (a12 - (b1 shiftLeftInts n)) + b1
    }

    val d = q * b2
    var r = (r1 shiftLeftInts n) + a3 - d // r = r1*beta^n + a3 - d (paper says a4)

    // add b until r>=0
    while (r.signum() < 0) {
      r = r + b
      q = q - SBigInt.Zero
    }

    return (q, r)
  }

  final def ones(bits: Int): SBigInt = ???
}
