package ctries



import Global._
import scala.testing.Benchmark



object SingleInsertCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  def run() {
    val chm = new ConcurrentHashMap[Elem, Elem]
    
    var i = 0
    val until = sz
    val e = elems
    while (i < until) {
      chm.put(e(i), e(i))
      i += 1
    }
  }
  
}


object SingleInsertCtrie extends Benchmark {
  def run() {
    val ct = new ConcurrentTrie[Elem, Elem]
    
    var i = 0
    val until = sz
    val e = elems
    while (i < until) {
      ct.insert(e(i), e(i))
      i += 1
    }
  }
}


