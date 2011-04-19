package ctries2




import org.scalatest._
import org.scalatest.matchers.ShouldMatchers



case class Wrap(i: Int) {
  override def hashCode = i * 0x9e3775cd
}


class CtrieSpec extends WordSpec with ShouldMatchers {
  
  "A ctrie2" should {
    
    "be created" in {
      val ct = new ConcurrentTrie
      ct
      println(ct.string)
    }
    
    "be inserted into" in {
      val ct = new ConcurrentTrie[Int, Int]
      ct.insert(5, 5)
      println(ct.string)
      assert(ct.lookupOpt(5) == Some(5))
    }
    
    "be inserted two values" in {
      val ct = new ConcurrentTrie[Int, Int]
      ct.insert(0, 5)
      ct.insert(1, 10)
      println(ct.string)
      assert(ct.lookupOpt(0) == Some(5))
      assert(ct.lookupOpt(1) == Some(10))
    }
    
    "be inserted many values" in {
      val sz = 1408
      val ct = new ConcurrentTrie[Int, Int]
      for (i <- 0 until sz) ct.insert(i, i)
      //println(ct.string)
      for (i <- 0 until sz) assert(ct.lookupOpt(i) == Some(i))
    }
    
    "be concurrently inserted non-overlapping values" in {
      val sz = 14080
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors
      
      class Inserter(mult: Int) extends Thread {
        override def run {
          for (i <- 0 until sz) ct.insert(new Wrap(i + mult * sz), i + mult * sz)
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Inserter(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until (nump * sz)) assert(ct.lookupOpt(new Wrap(i)) == Some(i), (i, ct.lookupOpt(new Wrap(i))))
    }
    
    "be concurrently inserted overlapping values" in {
      val sz = 125000
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors
      
      class Inserter(mult: Int) extends Thread {
        override def run {
          for (i <- 0 until sz) {
            val x = (i + mult * (sz / nump)) % sz
            ct.insert(new Wrap(x), x)
          }
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Inserter(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.lookupOpt(new Wrap(i)) == Some(i), (i, ct.lookupOpt(new Wrap(i))))
    }
    
    "be inserted into and removed from" in {
      val sz = 148
      val ct = new ConcurrentTrie[Int, Int]
      
      for (i <- 0 until sz) ct.insert(i, i)
      
      for (i <- (0 until sz).reverse) {
        val removedvalue = ct.remove(i)
        assert(removedvalue == Some(i), (i, removedvalue, ct.string))
        assert(ct.lookupOpt(i) == None, i)
        for (j <- 0 until i) assert(ct.lookupOpt(j) == Some(j), (j, ct.lookupOpt(j)))
      }
      
      println(ct.string)
    }
    
    "be inserted many values and removed from" in {
      val sz = 14800
      val ct = new ConcurrentTrie[Int, Int]
      
      for (i <- 0 until sz) ct.insert(i, i)
      
      for (i <- (0 until sz).reverse) {
        val removedvalue = ct.remove(i)
        assert(removedvalue == Some(i), (i, removedvalue))
        assert(ct.lookupOpt(i) == None, i)
      }
    }
    
    "be alternately inserted into and removed from" in {
      val sz = 32000
      val ct = new ConcurrentTrie[Int, Int]
      
      for (i <- 0 until sz) {
        ct.insert(i, i)
        if (i % 3 == 0) assert(ct.remove(i) == Some(i))
      }
      
      for (i <- 0 until sz) assert(ct.lookupOpt(i) == (if (i % 3 != 0) Some(i) else None))
    }
    
    def removeNonOverlapping(perthread: Int, nump: Int): ConcurrentTrie[Wrap, Int] = {
      val ct = new ConcurrentTrie[Wrap, Int]
      val sz = perthread * nump
      for (i <- 0 until sz) ct.insert(new Wrap(i), i)
      for (i <- 0 until sz) assert(ct.lookupOpt(new Wrap(i)) == Some(i))
      
      class Remover(mult: Int) extends Thread {
        override def run {
          for (i <- (mult * perthread) until (mult * perthread + perthread)) ct.remove(new Wrap(i))
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Remover(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.lookupOpt(new Wrap(i)) == None)
      ct
    }
    
    "be concurrently removed from with 128 non-overlapping values * procs, repeated 100 times" in {
      for (i <- 0 until 100) removeNonOverlapping(128, Runtime.getRuntime.availableProcessors)
    }
    
    "be concurrently removed from with 32k non-overlapping values * procs" in {
      val ct = removeNonOverlapping(32000, Runtime.getRuntime.availableProcessors)
      println(ct.string)
    }
    
    "be concurrently removed from with 256k non-overlapping values * procs" in {
      val ct = removeNonOverlapping(256000, Runtime.getRuntime.availableProcessors)
      println(ct.string)
    }
    
    "be concurrently removed from with 16k non-overlapping values * procs * 10" in {
      val ct = removeNonOverlapping(16000, 10 * Runtime.getRuntime.availableProcessors)
      println(ct.string)
    }
    
    "be concurrently removed from with overlapping values" in {
      val sz = 256000
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors * 2
      for (i <- 0 until sz) ct.insert(new Wrap(i), i)
      for (i <- 0 until sz) assert(ct.lookupOpt(new Wrap(i)) == Some(i))
      
      class Remover(mult: Int) extends Thread {
        override def run {
          for (i <- 0 until sz) {
            val x = (i + mult * (sz / nump)) % sz
            ct.remove(new Wrap(x))
          }
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Remover(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.lookupOpt(new Wrap(i)) == None)
      println(ct.string)
    }
    
    "be concurrently inserted into and removed from" in {
      val presz = 256000
      val chsz = 256000
      val totelems = presz + chsz
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors
      for (i <- 0 until presz) ct.insert(new Wrap(i), i)
      
      class Inserter(mult: Int) extends Thread {
        override def run {
          for (i <- 0 until chsz) ct.insert(new Wrap(presz + i), i)
        }
      }
      
      class Remover(offset: Int, total: Int, mult: Int) extends Thread {
        override def run {
          for (i <- 0 until total) ct.remove(new Wrap(offset + i))
        }
      }
      
      val inserters = for (i <- 0 until nump) yield new Inserter(i)
      val removers = for (i <- 0 until nump) yield new Remover(presz, chsz, i)
      val threads = inserters ++ removers
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until presz) assert(ct.lookupOpt(new Wrap(i)) == Some(i))
      
      val cleaners = for (i <- 0 until nump) yield new Remover(0, totelems, i)
      cleaners.foreach(_.start())
      cleaners.foreach(_.join())
      
      for (i <- 0 until totelems) assert(ct.lookupOpt(new Wrap(i)) == None)
      println(ct.string)
    }
    
  }
  
}
