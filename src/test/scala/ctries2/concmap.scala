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
    
    "support replace if mapped to a specific values, using several threads" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      val sz = 11000
      for (i <- 0 until sz) ct.update(new Wrap(i), i)
      
      class Updater(offs: Int) extends Thread {
        override def run {
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            var k = -1
            do {
              k = ct.lookup(new Wrap(j))
            } while (!ct.replace(new Wrap(k), k, -k))
          }
        }
      }
      
      val threads = for (i <- 0 until 16) yield new Updater(i)
      threads.foreach(_.start())
      threads.foreach(_.join())
    }
    
  }
}
