/** Dependencies */
resolvers ++= Seq("retronym-releases" at "http://retronym.github.com/repo/releases",
                  "retronym-snapshots" at "http://retronym.github.com/repo/snapshots")

libraryDependencies += "com.github.retronym" % "sbt-onejar_2.8.1" % "0.3-SNAPSHOT"

