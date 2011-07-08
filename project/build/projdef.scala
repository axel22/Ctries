


import sbt._
import Process._
import java.io._



class Ctries(info: ProjectInfo) extends DefaultProject(info) {
  
  /* deps */
  
  //val scalatest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT"
  
  /* config */
  
  override def testSourceRoots = super.testSourceRoots +++ (sourcePath / "bench")
  
  override def compileOptions = super.compileOptions ++ compileOptions("-Yinline")
  
  /* helpers */
  
  def loginfo(msg: String) = log.log(Level.Info, msg)
  
  def runsync(comm: String) {
    loginfo("running: %s".format(comm))
    comm !;
  }
  
  def benchcomm(args: String) = "java -Xmx1500m -Xms1500m -server -cp %s:%s:%s %s".format(
    packageTestJar,
    buildScalaInstance.libraryJar,
    jarPath,
    args)
  
  val plotresfile = File.createTempFile("plot", "out")
  plotresfile.deleteOnExit();
  
  val gplotflag = "--gnuplot"
  
  val totruns = 25
  
  def generatePlots(cases: Seq[String], paramtups: List[(String, List[String])], input: String, output: String) = for (((pname, _), i) <- paramtups.zipWithIndex) {
    loginfo("plotting against %s".format(pname))
    
    val plotfile = new File("tmp/generateplot")
    val contents = """
#!/usr/bin/gnuplot

set terminal png
set grid
set xlabel "%s"
set ylabel "time"
set out "./tmp/%s"

plot %s
""".format(
  pname.split('=')(0),
  output + i + ".png",
  (for (j <- 0 until cases.length) yield {
    """ "%s" using %d:%d title "%s" with linespoints""".format(
      input, 
      i + 1,
      paramtups.length + 1 + j,
      cases(j)
    )
  }).mkString(", ")
)
    
    val out = new java.io.FileWriter(plotfile)
    out.write(contents)
    out.close
    
    "gnuplot %s".format(plotfile) !;
  }
  
  /* tasks */
  
  lazy val justTestOnly = testQuickMethod(testCompileConditional.analysis, testOptions)(o => testTask(testFrameworks, testClasspath, testCompileConditional.analysis, o))
  
  lazy val justTest = testTask(testFrameworks, testClasspath, testCompileConditional.analysis, testOptions) dependsOn (compile)
  
  lazy val bench = task { args =>
    task {
      runsync(benchcomm(args.mkString(" ")))
      None
    } dependsOn (`package`, packageTest)
  } 
  
  lazy val benchBatch = task { args =>
    task {
      val (params, nonparams) = args.partition(_.startsWith("-D"))
      val (other, benches) = nonparams.partition(_.startsWith("--"))
      val othermap = Map(other.map(_.split('=')).map(x => (x(0), x(1))): _*)
      
      val paramtups = for {
        p <- params.toList
        val pts = p.split('=')
        val (name, vals) = (pts(0), pts(1).split(','))
      } yield (p, vals.toList)
      
      def forparams(paramlist: List[(String, List[String])], args: List[String], accs: String)(f: (List[String], String) => Unit): Unit = paramlist match {
        case head :: tail => for (v <- head._2) forparams(tail, args ::: List(v), "%s %s=%s".format(accs, head._1.split('=')(0), v))(f)
        case Nil => f(args, accs)
      }
      
      if (othermap contains (gplotflag)) {
        val gpf = ("tmp" / othermap(gplotflag)).asFile
        gpf.delete()
        gpf.createNewFile()
        
        forparams(paramtups, Nil, "") { (lst, s) =>
          plotresfile.delete()
          plotresfile.createNewFile()
          for (bench <- benches) {
            loginfo("running for: %s, %s".format(bench, s))
            val bcomm = benchcomm("%s %s %d 1".format(s, bench, totruns))
            //loginfo("command: %s".format(bcomm))
            bcomm #| """sed -e s/[^0-9\t]//g """.format(bench) #>> plotresfile !;
          }
          //"cat %s".format(plotresfile.getAbsolutePath) !;
          "echo -ne %s ".format(lst.mkString(" ")) #>> gpf !;
          runsync("tools/toplot %s %s".format(plotresfile.getAbsolutePath, gpf.toString))
        }
        
        generatePlots(benches, paramtups, gpf.getAbsolutePath, othermap(gplotflag))
      } else {
        forparams(paramtups, Nil, "") { (lst, s) =>
          for (bench <- benches) {
            runsync(benchcomm("%s %s %d 1".format(s, bench, totruns)))
          }
        }
      }
      
      None
    } dependsOn (`package`, packageTest)
  }
  
}
