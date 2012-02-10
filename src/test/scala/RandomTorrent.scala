package test.scala

import org.yaml.snakeyaml.Yaml
import scala.io.Source
import scala.collection.Map
import scala.collection.JavaConversions._

object RandomTorrent
{
  val testconfig = Source.fromURL(getClass.getResource("/testconfig.yaml"))
  var yaml : Yaml = new Yaml()
  val javamap : java.util.ArrayList[String] = (yaml.load(testconfig.mkString).asInstanceOf[java.util.ArrayList[String]])
	var lb : scala.collection.mutable.ListBuffer[String] = new scala.collection.mutable.ListBuffer[String]()
	
  for( a <- javamap )
		lb += a
  
  val urlList : List[String] = lb.toList
  var r = new scala.util.Random( (new java.util.Date()).getTime() )
  
  def get : String = {
		urlList( java.lang.Math.abs(r.nextInt()) % urlList.size() )
  }
}