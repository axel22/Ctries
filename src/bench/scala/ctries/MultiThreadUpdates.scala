package ctries



import Global._
import scala.testing.Benchmark



object MultiUpdateCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  var chm = new ConcurrentHashMap[Elem, Elem]
  val array = Array.fill(lookups.get)(0) ++ Array.fill(inserts.get)(1) ++ Array.fill(removes.get)(2)
  
  override def setUp() {
    chm = new ConcurrentHashMap[Elem, Elem]
    for (i <- 0 until sz) chm.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val howmany = totalops.get / p
    
    val ws = for (i <- 0 until p) yield new Worker(chm, i, howmany)
    
    for (i <- ws) i.start()
    for (i <- ws) i.join()
  }
  
  class Worker(chm: ConcurrentHashMap[Elem, Elem], n: Int, howmany: Int) extends Thread {
    override def run() {
      var i = 0
      val until = howmany
      val e = elems
      val arr = array
      val arrlen = array.length
      val s = sz
      
      while (i < until) {
        val imodsz = i % s
        array(i % arrlen) match {
          case 0 => chm.get(e(imodsz))
          case 1 => chm.put(e(imodsz), e(imodsz))
          case 2 => chm.remove(e(imodsz))
        }
        
        i += 1
      }
    }
  }
}


object MultiUpdateSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  var csl = new ConcurrentSkipListMap[Elem, Elem]
  val array = Array.fill(lookups.get)(0) ++ Array.fill(inserts.get)(1) ++ Array.fill(removes.get)(2)
  
  override def setUp() {
    csl = new ConcurrentSkipListMap[Elem, Elem]
    for (i <- 0 until sz) csl.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val howmany = totalops.get / p
    
    val ws = for (i <- 0 until p) yield new Worker(csl, i, howmany)
    
    for (i <- ws) i.start()
    for (i <- ws) i.join()
  }
  
  class Worker(csl: ConcurrentSkipListMap[Elem, Elem], n: Int, howmany: Int) extends Thread {
    override def run() {
      var i = 0
      val until = howmany
      val e = elems
      val arr = array
      val arrlen = array.length
      val s = sz
      
      while (i < until) {
        val imodsz = i % s
        array(i % arrlen) match {
          case 0 => csl.get(e(imodsz))
          case 1 => csl.put(e(imodsz), e(imodsz))
          case 2 => csl.remove(e(imodsz))
        }
        
        i += 1
      }
    }
  }
}


object MultiUpdateCtrie2 extends Benchmark {
  import ctries2.ConcurrentTrie
  
  var ct = new ConcurrentTrie[Elem, Elem]
  val array = Array.fill(lookups.get)(0) ++ Array.fill(inserts.get)(1) ++ Array.fill(removes.get)(2)
  
  override def setUp() {
    ct = new ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val howmany = totalops.get / p
    
    val ws = for (i <- 0 until p) yield new Worker(ct, i, howmany)
    
    for (i <- ws) i.start()
    for (i <- ws) i.join()
  }
  
  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, howmany: Int) extends Thread {
    override def run() {
      var i = 0
      val until = howmany
      val e = elems
      val arr = array
      val arrlen = array.length
      val s = sz
      
      while (i < until) {
        val imodsz = i % s
        array(i % arrlen) match {
          case 0 => ct.lookup(e(imodsz))
          case 1 => ct.update(e(imodsz), e(imodsz))
          case 2 => ct.remove(e(imodsz))
        }
        
        i += 1
      }
    }
  }
}


