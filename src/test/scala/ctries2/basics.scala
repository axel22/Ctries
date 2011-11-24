package ctries2



import org.scalacheck._
import Prop._
import org.scalacheck.Gen._
import collection._



class CtrieChecks extends Properties("Ctrie") {
  
  /* generators */
  
  val sizes = choose(0, 200000)
  
  val threadCounts = choose(2, 16)
  
  val threadCountsAndSizes = for {
    p <- threadCounts
    sz <- sizes
  } yield (p, sz);
  
  
  /* helpers */
  
  def inParallel[T](totalThreads: Int)(body: Int => T): Seq[T] = {
    val threads = for (idx <- 0 until totalThreads) yield new Thread {
      private var res: T = _
      override def run() {
        res = body(idx)
      }
      def result = {
        this.join()
        res
      }
    }
    
    threads foreach (_.start())
    threads map (_.result)
  }
  
  def spawn[T](body: =>T): { def get: T } = {
    val t = new Thread {
      private var res: T = _
      override def run() {
        res = body
      }
      def result = res
    }
    t.start()
    new {
      def get: T = {
        t.join()
        t.result
      }
    }
  }
  
  def elementRange(threadIdx: Int, totalThreads: Int, totalElems: Int): Range = {
    val sz = totalElems
    val idx = threadIdx
    val p = totalThreads
    val start = (sz / p) * idx + math.min(idx, sz % p)
    val elems = (sz / p) + (if (idx < sz % p) 1 else 0)
    val end = start + elems
    (start until end)
  }
  
  def hasGrown[K, V](last: Map[K, V], current: Map[K, V]) = {
    (last.size <= current.size) && {
      last forall {
        case (k, v) => current.get(k) == Some(v)
      }
    }
  }
  
  object err {
    var buffer = new StringBuilder
    def println(a: AnyRef) = buffer.append(a.toString).append("\n")
    def clear() = buffer.clear()
    def flush() = {
      Console.out.println(buffer)
      clear()
    }
  }
  
  
  /* properties */
  
  property("concurrent growing snapshots") = forAll(threadCounts, sizes) {
    (numThreads, numElems) =>
    val p = 9 //numThreads
    val sz = 128 //numElems
    val ct = new ConcurrentTrie[Wrap, Int]
    err.println("----------------------")
    
    // checker
    val future = spawn {
      def check(last: Map[Wrap, Int], iterationsLeft: Int): Boolean = {
        val current = ct.readOnlySnapshot()
        err.println("new check! " + current.size + " / " + sz)
        if (!hasGrown(last, current)) false
        else if (current.size == sz) true
        else if (iterationsLeft < 0) false
        else check(current, iterationsLeft - 1)
      }
      check(ct.readOnlySnapshot(), 100)
    }
    
    // fillers
    inParallel(p) {
      idx =>
      elementRange(idx, p, sz) foreach (i => ct.put(Wrap(i), i))
    }
    
    // wait for checker to finish
    val growing = future.get
    if (!growing) {
      err.println("size: " + ct.size)
      err.println(ct.toList.map(_._2).sorted.zip(0 until sz).find(p => p._1 != p._2))
      err.flush()
    } else err.clear()
    
    growing && ((0 until sz) forall {
      case i => ct.get(Wrap(i)) == Some(i)
    })
  }
  
  /*
  property("update") = forAll(sizes) {
    (n: Int) =>
    val ct = new ConcurrentTrie[Int, Int]
    for (i <- 0 until n) ct(i) = i
    (0 until n) forall {
      case i => ct(i) == i
    }
  }
  
  
  property("concurrent update") = forAll(threadCountsAndSizes) {
    case (p, sz) =>
      val ct = new ConcurrentTrie[Wrap, Int]
      
      inParallel(p) {
        idx =>
        for (i <- elementRange(idx, p, sz)) ct(Wrap(i)) = i
      }
      
      (0 until sz) forall {
        case i => ct(Wrap(i)) == i
      }
  }
  
  
  property("concurrent remove") = forAll(threadCounts, sizes) {
    (p, sz) =>
    val ct = new ConcurrentTrie[Wrap, Int]
    for (i <- 0 until sz) ct(Wrap(i)) = i
    
    inParallel(p) {
      idx =>
      for (i <- elementRange(idx, p, sz)) ct.remove(Wrap(i))
    }
    
    (0 until sz) forall {
      case i => ct.get(Wrap(i)) == None
    }
  }
  
  
  property("concurrent putIfAbsent") = forAll(threadCounts, sizes) {
    (p, sz) =>
    val ct = new ConcurrentTrie[Wrap, Int]
    
    val results = inParallel(p) {
      idx =>
      elementRange(idx, p, sz) find (i => ct.putIfAbsent(Wrap(i), i) != None)
    }
    
    (results forall (_ == None)) && ((0 until sz) forall {
      case i => ct.get(Wrap(i)) == Some(i)
    })
  }
  */
  
}










