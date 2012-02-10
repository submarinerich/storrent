package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll

class ByteTester extends FunSuite with ShouldMatchers {
	
  import com.submarinerich.storrent.util.ByteUtils

  test("make sure ints work like you think"){
    var a = 183180
    var ba = ByteUtils.toByteArray(a)
    var b = ByteUtils.toInt(ba)
    a should equal (b)
  }
}