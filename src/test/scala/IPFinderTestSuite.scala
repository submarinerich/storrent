package test.scala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll

class IPFinderTestSuite extends FunSuite with ShouldMatchers
{
	import com.submarinerich.storrent.util.IPFinder
  test("finding my ip")
  {
  	var foundIP = false
    IPFinder.thisIP match {
      case Some( str : String ) => {
        str.size should be > (10)
        foundIP = true
        Console.println("ip address is :"+str)
      }
      case _ => foundIP = false
    }
    foundIP should equal (true)
  }
}