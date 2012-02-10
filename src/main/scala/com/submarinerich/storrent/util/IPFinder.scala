package com.submarinerich.storrent.util

import org.apache.http.impl.nio.client.DefaultHttpAsyncClient
import org.apache.http.nio.client.HttpAsyncClient
import org.apache.http.client.methods.HttpGet
import scala.io.Source
import scala.actors.Futures._

object IPFinder
{
  def thisIP : Option[String] = {
    var httpclient = new DefaultHttpAsyncClient()

    httpclient.start()
    
    var ipstring : Option[String] = None

    try{
      var request = new HttpGet("http://whatismyip.org")
      var future = httpclient.execute(request,null)
      var response = future.get()
      if( response.getStatusLine().getStatusCode() == 200 )
        ipstring = Some( Source.createBufferedSource( response.getEntity().getContent() ).mkString )
    }finally{
      httpclient.shutdown()
    }
    ipstring
  }
}