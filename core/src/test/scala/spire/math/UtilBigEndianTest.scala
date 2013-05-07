package spire.math

import org.scalatest.FunSuite
import spire.implicits.{ eqOps ⇒ _, _ }
import java.math.BigInteger

import UtilBigEndian._

class UtilBigEndianTest extends FunSuite {
  test("inplaceAdd") {
    val input = List(Array(1), Array(1, 0), Array(0, 1), Array(1, 1), Array(1, 1, 1))

    for (i <- input) {
      inplaceAdd(i, 1, 1)
    }

    val expected = List(List(2), List(1, 1), List(0, 2), List(1, 2), List(1, 1, 2))

    for (i <- input.zipWithIndex) {
      assert(i._1.toList === expected(i._2), s"index: ${i._2}")
    }
  }

  test("removeLeadingZeroes") {
    val input: Array[Array[Int]] = Array(Array(), Array(0), Array(0, 0), Array(0, 1, 2, 3), Array(0, 1, 0), Array(0, 0, -1, 0, 1, 2, 3))
    val expected: Array[Array[Int]] = Array(Array(), Array(), Array(), Array(1, 2, 3), Array(1, 0), Array(-1, 0, 1, 2, 3))

    input.indices foreach { i =>
      val newArr = removeLeadingZeroes(input(i))
      assert(newArr === expected(i))
    }
  }

  test("arrayPlusArray — [1] + [2]") {
    val result = arrayPlusArray(Array(1), Array(2))
    assert(result.toList === List(3))
  }

  test("arrayPlusArray — [] + []") {
    val result = arrayPlusArray(Array[Int](), Array[Int]())
    assert(result.toList === List())
  }

  test("arrayPlusArray — [1] + []") {
    val result = arrayPlusArray(Array(1), Array[Int]())
    assert(result.toList === List(1))
  }

  test("arrayPlusArray — [1,1] + [1]") {
    val result = arrayPlusArray(Array(1, 1), Array(1))
    assert(result.toList === List(1, 2))
  }

  test("arrayPlusArray — [1,2,3] + [1]") {
    val result = arrayPlusArray(Array(1, 2, 3), Array(1))
    assert(result.toList === List(1, 2, 4))
  }

  test("arrayPlusArray — [1,2,3,4,5] + [9,8,7]") {
    val result = arrayPlusArray(Array(1, 2, 3, 4, 5), Array(9, 8, 7))
    assert(result.toList === List(1, 2, 12, 12, 12))
  }

  test("arrayPlusArray with overflow — [-1] + [1]") {
    val result = arrayPlusArray(Array(-1), Array(1))
    assert(result.toList === List(1, 0))
  }

  test("arrayPlusArray with overflow — [-1] + [-1]") {
    val result = arrayPlusArray(Array(-1), Array(-1))
    assert(result.toList === List(1, -2), result)
  }

  test("arrayPlusArray with overflow — [-1,-1,-1] + [1]") {
    val result = arrayPlusArray(Array(-1, -1, -1), Array(1))
    assert(result.toList === List(1, 0, 0, 0), result)
  }

  test("arrayPlusArray with overflow — [-1,0,-1,-1] + [1]") {
    val result = arrayPlusArray(Array(-1, 0, -1, -1), Array(1))
    assert(result.toList === List(-1, 1, 0, 0), result)
  }

  test("arrayMinusArray — [42] - [42]") {
    val result = arrayMinusArray(Array(42), Array(42))
    assert(result.toList === List())
  }

  test("arrayMinusArray — [2] - [1]") {
    val result = arrayMinusArray(Array(2), Array(1))
    assert(result.toList === List(1))
  }

  test("arrayMinusArray — [] - []") {
    val result = arrayMinusArray(Array[Int](), Array[Int]())
    assert(result.toList === List())
  }

  test("arrayMinusArray — [1] - []") {
    val result = arrayMinusArray(Array(1), Array[Int]())
    assert(result.toList === List(1))
  }

  test("arrayMinusArray — [1,1] - [1]") {
    val result = arrayMinusArray(Array(1, 1), Array(1))
    assert(result.toList === List(1, 0))
  }

  test("arrayMinusArray — [1,2,3] - [1]") {
    val result = arrayMinusArray(Array(1, 2, 3), Array(1))
    assert(result.toList === List(1, 2, 2))
  }
}
