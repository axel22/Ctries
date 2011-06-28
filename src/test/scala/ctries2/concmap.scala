package ctries2



import org.scalatest._
import org.scalatest.matchers.ShouldMatchers



class ConcurrentMapSpec extends WordSpec with ShouldMatchers {
  
  // TODO implement and finish
  
  "A ctrie2" should {
    
    "support put if absent" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.insert(new Wrap(i), i)
    }
    
    "support remove if mapped to a specific value" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.insert(new Wrap(i), i)
    }
    
    "support replace if mapped to a specific value" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.insert(new Wrap(i), i)
    }
    
    "support replace if present" in {
      val ct = new ConcurrentTrie[Wrap, Int]
      for (i <- 0 until 450) ct.insert(new Wrap(i), i)
    }
    
  }
}
