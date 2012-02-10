
require 'build/library.rb'

def buildClasspath()
  sh "mvn dependency:build-classpath"
  classpath = ""
  file = File.new("classpath.txt", "r")
  while (line = file.gets)
    classpath += line
  end
  file.close
  return classpath
end

desc "install"
task :install => :package do 
  if !File.directory?(File.join(ENV['HOME'],".storrent"))
    Dir.mkdir(File.join(ENV['HOME'], ".storrent"), 0700)
  end
  oneJar = "storrent-"+version()+".one-jar.jar"
  File.delete('bin/storrent')
  File.open('bin/storrent','w+') { |f| 
    f.write("#!/bin/bash\n")
    f.write("LAUNCHJAR=~/.storrent/"+oneJar+"\n")
    f.write("exec java -jar $LAUNCHJAR com.submarinerich.storrent.TorrentClient \"$@\"\n")
    f.close
  }
  sh "cp target/"+oneJar+" ~/.storrent/"
  sh "cp bin/storrent /opt/local/bin/"
  sh "chmod 755 /opt/local/bin/storrent"
end



desc "download"
task :download, :url do | t,arg |
  classpath = buildClasspath()
  sh "scala -classpath "+classpath+":target/classes/ com.submarinerich.storrent.TorrentClient "+arg.url+" download/"
end

desc "run" 
task :run => ["compile"] do
  classpath = buildClasspath()
  url = "http://releases.ubuntu.com/11.10/ubuntu-11.10-server-amd64.iso.torrent"
  sh "scala -classpath "+classpath+":target/classes/ com.submarinerich.storrent.TorrentClient "+url+" download/"
end


