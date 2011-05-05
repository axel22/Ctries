package ctries



import Global._
import scala.testing.Benchmark



object AfterDeleteCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  def run() {
    val chm = new ConcurrentHashMap[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) chm.put(e(i), e(i))
    for (i <- 0 until sz) chm.remove(e(i))
    
    while (true) {}
  }
}


object AfterDeleteSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  def run() {
    val skiplist = new ConcurrentSkipListMap[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) skiplist.put(e(i), e(i))
    for (i <- 0 until sz) skiplist.remove(e(i))
    
    while (true) {}
  }
}


object AfterDeleteCtrie extends Benchmark {
  def run() {
    val ctrie = new ctries.ConcurrentTrie[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) ctrie.insert(e(i), e(i))
    for (i <- 0 until sz) ctrie.remove(e(i))
    
    while (true) {}
  }
}


object AfterDeleteCtrie2 extends Benchmark {
  def run() {
    val ctrie = new ctries2.ConcurrentTrie[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) ctrie.insert(e(i), e(i))
    for (i <- 0 until sz) ctrie.remove(e(i))
    
    while (true) {}
  }
}


