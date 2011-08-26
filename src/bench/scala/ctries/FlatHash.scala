package ctries



import Global._
import scala.testing.Benchmark



object FlatHash extends Benchmark {
  import java.util.concurrent.atomic.AtomicReferenceArray
  
  var arr: AtomicReferenceArray[Elem] = null
  val loadFactor = 0.45
  
  override def setUp {
    arr = null
    arr = new AtomicReferenceArray[Elem]((sz / loadFactor).toInt)
    Runtime.getRuntime.gc()
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(arr, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(arr: AtomicReferenceArray[Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        flat_insert(arr, e(i))
        i += 1
      }
    }
  }
  
  @inline def flat_insert(arr: AtomicReferenceArray[Elem], e: Elem) {
    val len = arr.length
    var idx = e.hashCode % len
    if (idx < 0) idx = -idx
    
    do {
      while (arr.get(idx) != null) idx = (idx + 1) % len
      if (arr.weakCompareAndSet(idx, null, e)) idx = -1
    } while (idx != -1)
  }
  
}



object FlatHashNoComm extends Benchmark {
  import java.util.concurrent.atomic.AtomicReferenceArray
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(0, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(var s: Int = 0, n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        s += i * i % (n + 1) + step
        i += 1
      }
    }
  }
  
}

