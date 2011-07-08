package ctries



import Global._
import scala.testing.Benchmark



object IterationCtrie2 extends Benchmark {
  import ctries2.ConcurrentTrie
  
  val ct = new ConcurrentTrie[Elem, Elem]
  for (i <- 0 until sz) ct.put(elems(i), elems(i))
  
  def run() {
    val it = ct.iterator
    
    while (it.hasNext) it.next()
  }
  
}


// should be the same as iteration of a ctrie2
object IterationCtrie2ReadOnly extends Benchmark {
  import ctries2.ConcurrentTrie
  
  val ct = new ConcurrentTrie[Elem, Elem]
  for (i <- 0 until sz) ct.put(elems(i), elems(i))
  val ro = ct.readOnlySnapshot()
  
  def run() {
    val it = ro.iterator
    
    while (it.hasNext) it.next()
  }
  
}


object IterationSkipList extends Benchmark {
  val csl = new java.util.concurrent.ConcurrentSkipListMap[Elem, Elem]
  for (i <- 0 until sz) csl.put(elems(i), elems(i))
  
  def run() {
    val it = csl.iterator
    
    while (it.hasNext) it.next()
  }
  
}


object IterationCHM extends Benchmark {
  val chm = new java.util.concurrent.ConcurrentHashMap[Elem, Elem]
  for (i <- 0 until sz) chm.put(elems(i), elems(i))
  
  def run() {
    val it = chm.iterator
    
    while (it.hasNext) it.next()
  }
  
}


object IterationHashTrie extends Benchmark {
  var ht = collection.immutable.HashMap[Elem, Elem]()
  for (i <- 0 until sz) ht = ht + ((elems(i), elems(i)))
  
  def run() {
    val it = ht.iterator
    
    while (it.hasNext) it.next()
  }
}


object IterationHashTable extends Benchmark {
  val hm = collection.mutable.HashMap[Elem, Elem]()
  for (i <- 0 until sz) hm(elems(i)) = elems(i)
  
  def run() {
    val it = hm.iterator
    
    while (it.hasNext) it.next()
  }
}


object IterationList extends Benchmark {
  var lst = List[(Elem, Elem)]()
  for (i <- 0 until sz) lst ::= (elems(i), elems(i))
  
  def run() {
    val it = lst.iterator
    
    while (it.hasNext) it.next()
  }
}


object IterationArray extends Benchmark {
  var arr = new Array[(Elem, Elem)](sz)
  for (i <- 0 until sz) arr(i) = (elems(i), elems(i))
  
  def run() {
    val it = arr.iterator
    
    while (it.hasNext) it.next()
  }
}
