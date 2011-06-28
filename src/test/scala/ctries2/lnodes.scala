package ctries2



import org.scalatest._
import org.scalatest.matchers.ShouldMatchers



class DumbHash(val i: Int) {
  override def equals(other: Any) = other match {
    case that: DumbHash => that.i == this.i
    case _ => false
  }
  override def hashCode = i % 5
  override def toString = "DH(%s)".format(i)
}


class LNodeSpec extends WordSpec with ShouldMatchers {
  
  "A ctrie2" should {
    
    "accept elements with the same hash codes" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 50) ct.update(new DumbHash(i), i)
    }
    
    "lookup elements with the same hash codes" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 200) ct.update(new DumbHash(i), i)
      for (i <- 0 until 200) assert(ct.get(new DumbHash(i)) == Some(i))
      for (i <- 200 until 250) assert(ct.get(new DumbHash(i)) == None)
    }
    
    "remove elements with the same hash codes" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 200) ct.update(new DumbHash(i), i)
      for (i <- 0 until 200) assert(ct.remove(new DumbHash(i)) == Some(i))
      for (i <- 0 until 200) assert(ct.get(new DumbHash(i)) == None)
    }
    
    "put elements with the same hash codes if absent" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 200) ct.put(new DumbHash(i), i)
      for (i <- 0 until 200) assert(ct.lookup(new DumbHash(i)) == i)
      for (i <- 0 until 200) assert(ct.putIfAbsent(new DumbHash(i), i) == Some(i))
      for (i <- 200 until 300) assert(ct.putIfAbsent(new DumbHash(i), i) == None)
      for (i <- 200 until 300) assert(ct.lookup(new DumbHash(i)) == i)
    }
    
    "replace elements with the same hash codes" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 200) assert(ct.put(new DumbHash(i), i) == None)
      for (i <- 0 until 200) assert(ct.lookup(new DumbHash(i)) == i)
      for (i <- 0 until 200) assert(ct.replace(new DumbHash(i), -i) == Some(i))
      for (i <- 0 until 200) assert(ct.lookup(new DumbHash(i)) == -i)
      for (i <- 0 until 200) assert(ct.replace(new DumbHash(i), -i, i) == true)
    }
    
    "remove elements with the same hash codes if mapped to a specific value" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 200) assert(ct.put(new DumbHash(i), i) == None)
      for (i <- 0 until 200) assert(ct.remove(new DumbHash(i), i) == true)
    }
    
  }
  
}
