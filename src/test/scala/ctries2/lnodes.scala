package ctries2



import org.scalatest._
import org.scalatest.matchers.ShouldMatchers



class DumbHash(val i: Int) {
  override def equals(other: Any) = other match {
    case that: DumbHash => that.i == this.i
    case _ => false
  }
  override def hashCode = i % 5
}


class LNodeSpec extends WordSpec with ShouldMatchers {
  
  "A ctrie2" should {
    
    "accept elements with the same hash codes" in {
      val ct = new ConcurrentTrie[DumbHash, Int]
      for (i <- 0 until 50) ct.insert(new DumbHash(i), i)
    }
    
  }
}
