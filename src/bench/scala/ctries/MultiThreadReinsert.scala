package ctries



import Global._
import scala.testing.Benchmark



object MultiReinsertCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  var chm = new ConcurrentHashMap[Elem, Elem]
  
  override def setUp {
    chm = new ConcurrentHashMap[Elem, Elem]
    for (i <- 0 until sz) chm.put(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(chm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        chm.put(e(i), e(i))
        i += 1
      }
    }
  }
}


object MultiReinsertSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  var skiplist = new ConcurrentSkipListMap[Elem, Elem]
  
  override def setUp {
    skiplist = new ConcurrentSkipListMap[Elem, Elem]
    for (i <- 0 until sz) skiplist.put(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(skiplist, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(skiplist: ConcurrentSkipListMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        skiplist.put(e(i), e(i))
        i += 1
      }
    }
  }
}


object MultiReinsertCtrie extends Benchmark {
  var ct = new ctries.ConcurrentTrie[Elem, Elem]
  
  override def setUp {
    ct = new ctries.ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.insert(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(ct: ctries.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.insert(e(i), e(i))
        i += 1
      }
    }
  }
}


object MultiReinsertCtrie2 extends Benchmark {
  var ct = new ctries2.ConcurrentTrie[Elem, Elem]
  
  override def setUp {
    ct = new ctries2.ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.insert(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(ct: ctries2.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.insert(e(i), e(i))
        i += 1
      }
    }
  }
}


