package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0,1).swap== Pair(1,0))
    assert((Pair("Rahul","don").swap== Pair("don","Rahul")))
  }

  test("Pair map") {
    assert(Pair(0,1).map(identity)== Pair(0,1))
  }

  test("Pair decoded") {
    val tmp= secret.map(_.reverse)
    assert(tmp==Pair("", ""))
  }

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
     assert((isEven && isPositive)(12) == true )
     assert((isEven && isPositive)(11) == false)
     assert((isEven && isPositive)(-4) == false)
     assert((isEven && isPositive)(-7) == false)
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12) == true)
    assert((isEven || isPositive)(11) == true)
    assert((isEven || isPositive)(-4) == true)
    assert((isEven || isPositive)(-7) == false)
  }

  test("Predicate flip") {
    assert(isEven.flip(11) == true)
    assert(isPositive.flip(5)==false)
    assert(isEven.flip(2)==false)
  }

  test("Predicate isValidUser"){
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false)
    assert(isValidUser(User("john", 20)) == false)
    assert(isValidUser(User("x"   , 23)) == false)
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode(json="123")==UserId(123))
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder weirdLocalDateDecoder") {}

}
