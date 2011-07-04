package ctries2




import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import annotation.tailrec



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
    
    "be updated into" in {
      val ct = new ConcurrentTrie[Int, Int]
      ct.update(5, 5)
      println(ct.string)
      assert(ct.get(5) == Some(5))
    }
    
    "be updated two values" in {
      val ct = new ConcurrentTrie[Int, Int]
      ct.update(0, 5)
      ct.update(1, 10)
      println(ct.string)
      assert(ct.get(0) == Some(5))
      assert(ct.get(1) == Some(10))
    }
    
    "be updated many values" in {
      val sz = 1408
      val ct = new ConcurrentTrie[Int, Int]
      for (i <- 0 until sz) ct.update(i, i)
      //println(ct.string)
      for (i <- 0 until sz) assert(ct.get(i) == Some(i))
    }
    
    "be concurrently updated non-overlapping values" in {
      val sz = 14080
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors
      
      class Updateer(mult: Int) extends Thread {
        override def run() {
          for (i <- 0 until sz) ct.update(new Wrap(i + mult * sz), i + mult * sz)
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Updateer(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until (nump * sz)) assert(ct.get(new Wrap(i)) == Some(i), (i, ct.get(new Wrap(i))))
    }
    
    "be concurrently updated overlapping values" in {
      val sz = 125000
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors
      
      class Updateer(mult: Int) extends Thread {
        override def run() {
          for (i <- 0 until sz) {
            val x = (i + mult * (sz / nump)) % sz
            ct.update(new Wrap(x), x)
          }
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Updateer(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == Some(i), (i, ct.get(new Wrap(i))))
    }
    
    "be updated into and removed from" in {
      val sz = 148
      val ct = new ConcurrentTrie[Int, Int]
      
      for (i <- 0 until sz) ct.update(i, i)
      
      for (i <- (0 until sz).reverse) {
        val removedvalue = ct.remove(i)
        assert(removedvalue == Some(i), (i, removedvalue, ct.string))
        assert(ct.get(i) == None, i)
        for (j <- 0 until i) assert(ct.get(j) == Some(j), (j, ct.get(j)))
      }
      
      println(ct.string)
    }
    
    "be updated many values and removed from" in {
      val sz = 14800
      val ct = new ConcurrentTrie[Int, Int]
      
      for (i <- 0 until sz) ct.update(i, i)
      
      for (i <- (0 until sz).reverse) {
        val removedvalue = ct.remove(i)
        assert(removedvalue == Some(i), (i, removedvalue))
        assert(ct.get(i) == None, i)
        //if (i < 40) println(ct.string)
      }
    }
    
    "be alternately updated into and removed from" in {
      val sz = 32000
      val ct = new ConcurrentTrie[Int, Int]
      
      for (i <- 0 until sz) {
        ct.update(i, i)
        if (i % 3 == 0) assert(ct.remove(i) == Some(i))
      }
      
      for (i <- 0 until sz) assert(ct.get(i) == (if (i % 3 != 0) Some(i) else None))
    }
    
    def removeNonOverlapping(perthread: Int, nump: Int): ConcurrentTrie[Wrap, Int] = {
      val ct = new ConcurrentTrie[Wrap, Int]
      val sz = perthread * nump
      for (i <- 0 until sz) ct.update(new Wrap(i), i)
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == Some(i))
      
      class Remover(mult: Int) extends Thread {
        override def run() {
          for (i <- (mult * perthread) until (mult * perthread + perthread)) ct.remove(new Wrap(i))
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Remover(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == None)
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
      for (i <- 0 until sz) ct.update(new Wrap(i), i)
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == Some(i))
      
      class Remover(mult: Int) extends Thread {
        override def run() {
          for (i <- 0 until sz) {
            val x = (i + mult * (sz / nump)) % sz
            ct.remove(new Wrap(x))
          }
        }
      }
      
      val threads = for (i <- 0 until nump) yield new Remover(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == None)
      println(ct.string)
    }
    
    "be concurrently updated into and removed from" in {
      val presz = 256000
      val chsz = 256000
      val totelems = presz + chsz
      val ct = new ConcurrentTrie[Wrap, Int]
      val nump = Runtime.getRuntime.availableProcessors
      for (i <- 0 until presz) ct.update(new Wrap(i), i)
      
      class Updater(mult: Int) extends Thread {
        override def run() {
          for (i <- 0 until chsz) ct.update(new Wrap(presz + i), i)
        }
      }
      
      class Remover(offset: Int, total: Int, mult: Int) extends Thread {
        override def run() {
          for (i <- 0 until total) ct.remove(new Wrap(offset + i))
        }
      }
      
      val updaters = for (i <- 0 until nump) yield new Updater(i)
      val removers = for (i <- 0 until nump) yield new Remover(presz, chsz, i)
      val threads = updaters ++ removers
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until presz) assert(ct.get(new Wrap(i)) == Some(i))
      
      val cleaners = for (i <- 0 until nump) yield new Remover(0, totelems, i)
      cleaners.foreach(_.start())
      cleaners.foreach(_.join())
      
      for (i <- 0 until totelems) assert(ct.get(new Wrap(i)) == None)
      println(ct.string)
    }
    
    "be concurrently emptied and then filled" in {
      val sz = 1 << 18
      val nump = 16
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until sz) ct.update(new Wrap(i), i)
      
      class Worker(i: Int) extends Thread {
        override def run() {
          val start = i * sz / nump
          val end = start + sz / nump
          for (i <- start until end) ct.remove(new Wrap(i))
          ct.put(new Wrap(-i), -i)
          
          @tailrec def barrier() {
            if ((0 until nump).exists(i => ct.get(new Wrap(-i)) == None)) barrier()
          }
          barrier()
          
          for (i <- 0 until sz) ct.put(new Wrap(i), -i)
        }
      }
      
      val workers = for (i <- 0 until nump) yield new Worker(i)
      workers.foreach(_.start())
      workers.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == Some(-i))
      for (i <- 0 until nump) assert(ct.get(new Wrap(-i)) == Some(-i))
    }
    
    "be concurrently looked up, emptied and filled" in {
      val R = 5
      val W = 10
      val filltimes = 10
      val readtimes = 100
      val sz = 145000
      val ct = new ConcurrentTrie[Wrap, Int]
      
      class Reader extends Thread {
        override def run() {
          for (k <- 0 until readtimes) {
            for (i <- 0 until sz) ct.get(new Wrap(i))
          }
        }
      }
      
      class Filler extends Thread {
        override def run() {
          for (i <- 0 until sz) ct.put(new Wrap(i), i)
        }
      }
      
      class Remover extends Thread {
        override def run() {
          for (i <- 0 until sz) ct.remove(new Wrap(i))
        }
      }
      
      def fill() {
        val fs = for (i <- 0 until W) yield new Filler
        fs.foreach(_.start())
        fs.foreach(_.join())
      }
      
      def empty() {
        val rs = for (i <- 0 until W) yield new Remover
        rs.foreach(_.start())
        rs.foreach(_.join())
      }
      
      def checkEmpty = for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == None)
      
      fill()
      
      val readers = for (i <- 0 until R) yield new Reader
      readers.foreach(_.start())
      
      for (i <- 0 until filltimes) {
        empty()
        fill()
      }
      empty()
      
      checkEmpty
      readers.foreach(_.join())
      checkEmpty
    }
    
  }
  
}




























