package com.submarinerich.storrent

import java.io.File
import java.nio.channels.FileChannel
import java.io.FileOutputStream
import grizzled.slf4j.Logger

case class TorrentFile( thepath : String, fileIndex : Int )
{
  var log = Logger( classOf[TorrentFile] )
  var path : String = thepath
  var length : Long = 0
  var index : Int = fileIndex
  var open : Boolean = false
	
	override def toString : String = path+" length: "+length
}