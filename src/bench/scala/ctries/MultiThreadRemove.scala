package ctries



import Global._
import scala.testing.Benchmark


// uncomment setUp
object MultiRemoveCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  var chm = new ConcurrentHashMap[Elem, Elem]
  
  override def setUp {
    chm = new ConcurrentHashMap[Elem, Elem]
    for (i <- 0 until sz) chm.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(chm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  var last: AnyRef = null
  override def tearDown {
    last = chm
  }
  
  class Remover(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        chm.remove(e(i))
        i += 1
      }
    }
  }
}


object MultiRemoveSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  var skiplist = new ConcurrentSkipListMap[Elem, Elem]
  
  override def setUp {
    skiplist = new ConcurrentSkipListMap[Elem, Elem]
    for (i <- 0 until sz) skiplist.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(skiplist, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(skiplist: ConcurrentSkipListMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        skiplist.remove(e(i))
        i += 1
      }
    }
  }
}


object MultiRemoveCtrie extends Benchmark {
  var ct = new ctries.ConcurrentTrie[Elem, Elem]
  
  override def setUp {
    ct = new ctries.ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.insert(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(ct: ctries.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.remove(e(i))
        i += 1
      }
    }
  }
}


object MultiRemoveCtrie2 extends Benchmark {
  var ct = new ctries2.ConcurrentTrie[Elem, Elem]
  
  override def setUp {
    ct = new ctries2.ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.insert(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(ct: ctries2.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.remove(e(i))
        i += 1
      }
    }
  }
}


