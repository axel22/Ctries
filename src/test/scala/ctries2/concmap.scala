package ctries2



import org.scalatest._
import org.scalatest.matchers.ShouldMatchers



class ConcurrentMapSpec extends WordSpec with ShouldMatchers {
  
  "A ctrie2" should {
    
    "support put" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) assert(ct.put(new Wrap(i), i) == None)
      for (i <- 0 until 450) assert(ct.put(new Wrap(i), -i) == Some(i))
    }
    
    "support put if absent" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.update(new Wrap(i), i)
      for (i <- 0 until 450) assert(ct.putIfAbsent(new Wrap(i), -i) == Some(i))
      for (i <- 0 until 450) assert(ct.putIfAbsent(new Wrap(i), -i) == Some(i))
      for (i <- 450 until 500) assert(ct.putIfAbsent(new Wrap(i), -i) == None)
      for (i <- 450 until 500) assert(ct.putIfAbsent(new Wrap(i), i) == Some(-i))
    }
    
    "support remove if mapped to a specific value" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.update(new Wrap(i), i)
      for (i <- 0 until 450) assert(ct.remove(new Wrap(i), -i - 1) == false)
      for (i <- 0 until 450) assert(ct.remove(new Wrap(i), i) == true)
      for (i <- 0 until 450) assert(ct.remove(new Wrap(i), i) == false)
    }
    
    "support replace if mapped to a specific value" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.update(new Wrap(i), i)
      for (i <- 0 until 450) assert(ct.replace(new Wrap(i), -i - 1, -i - 2) == false)
      for (i <- 0 until 450) assert(ct.replace(new Wrap(i), i, -i - 2) == true)
      for (i <- 0 until 450) assert(ct.replace(new Wrap(i), i, -i - 2) == false)
      for (i <- 450 until 500) assert(ct.replace(new Wrap(i), i, 0) == false)
    }
    
    "support replace if present" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.update(new Wrap(i), i)
      for (i <- 0 until 450) assert(ct.replace(new Wrap(i), -i) == Some(i))
      for (i <- 0 until 450) assert(ct.replace(new Wrap(i), i) == Some(-i))
      for (i <- 450 until 500) assert(ct.replace(new Wrap(i), i) == None)
    }
    
    "support replace if mapped to a specific value, using several threads" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      val sz = 55000
      for (i <- 0 until sz) ct.update(new Wrap(i), i)
      
      class Updater(index: Int, offs: Int) extends Thread {
        override def run {
          var repeats = 0
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            var k = -1
            do {
              if (k != -1) repeats += 1
              k = ct.lookup(new Wrap(j))
            } while (!ct.replace(new Wrap(j), k, -k))
          }
          //println("Thread %d repeats: %d".format(index, repeats))
        }
      }
      
      val threads = for (i <- 0 until 16) yield new Updater(i, sz / 32 * i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct(new Wrap(i)) == i)
      
      val threads2 = for (i <- 0 until 15) yield new Updater(i, sz / 32 * i)
      threads2.foreach(_.start())
      threads2.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct(new Wrap(i)) == -i)
    }
    
    "support put if absent, several threads" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      val sz = 110000
      
      class Updater(offs: Int) extends Thread {
        override def run {
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            ct.putIfAbsent(new Wrap(j), j)
            assert(ct.lookup(new Wrap(j)) == j)
          }
        }
      }
      
      val threads = for (i <- 0 until 16) yield new Updater(sz / 32 * i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct(new Wrap(i)) == i)
    }
    
    "support remove if mapped to a specific value, several threads" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      val sz = 55000
      for (i <- 0 until sz) ct.update(new Wrap(i), i)
      
      class Remover(offs: Int) extends Thread {
        override def run {
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            ct.remove(new Wrap(j), j)
            assert(ct.get(new Wrap(j)) == None)
          }
        }
      }
      
      val threads = for (i <- 0 until 16) yield new Remover(sz / 32 * i)
      threads.foreach(_.start())
      threads.foreach(_.join())
      
      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == None)
    }
    
  }
}
