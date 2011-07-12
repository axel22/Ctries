package ctries



import Global._
import scala.testing.Benchmark
import scala.collection.parallel._



class ParCtrie[K, V](szest: Int) extends mutable.ParMap[K, V] {
  val seq = new ctries2.ConcurrentTrie[K, V]
  
  def clear() = throw new UnsupportedOperationException
  
  final def +=(kv: (K, V)) = {
    seq += kv
    this
  }
  
  final def remove(k: K): Option[V] = seq.remove(k)
  
  final def -=(k: K) = {
    seq -= k
    this
  }
  
  def splitter: IterableSplitter[(K, V)] =
    new CtrieSplitter(szest, seq.readOnlySnapshot().asInstanceOf[ctries2.ConcurrentTrie[K, V]]) with SCPI
  
  def get(k: K): Option[V] = seq.get(k)
  
  def put(key: K, value: V): Option[V] = seq.put(key, value)
  
  def size = szest
  
  type SCPI = SignalContextPassingIterator[CtrieSplitter]
  
  class CtrieSplitter(szestimate: Int, ct: ctries2.ConcurrentTrie[K, V])
  extends ctries2.CtrieIterator[K, V](ct) with ParIterator {
  self: SCPI =>
    def remaining = szestimate // not using these ops
    def dup = throw new UnsupportedOperationException // not using views
    def split: Seq[CtrieSplitter] = subdivide.map { // probably won't use LNodes
      case ci: ctries2.CtrieIterator[K, V] =>
        val cs = new CtrieSplitter(szestimate / 2, ct) with SCPI
        cs.stack = ci.stack
        cs.stackpos = ci.stackpos
        cs.depth = ci.depth
        cs.current = ci.current
        cs
    }
  }
  
}


// the total number of links on the page
// indices of the pages linked by this page
// and the indices of the pages linking to this page
case class Page(name: Int, linksnum: Int, var outgoing: Array[Int], var incoming: Array[Int]) {
  override def toString = "Page(%d, %d, %s, %s)".format(name, linksnum, outgoing.toList, incoming.toList)
}


object Page {
  val rand = new util.Random(324132L)
  
  // creates a set of pages with most links to middle pages
  def middlePages(sz: Int) = {
    val pages = (for (i <- 0 until sz) yield {
      val linknum = rand.nextInt(8)
      val outgoing = for (i <- 0 until linknum) yield {
        val lnk = ((rand.nextGaussian() / 4 + 0.5) * sz) max 0 min (sz - 1)
        lnk.toInt
      }
      Page(i, linknum, outgoing.toArray, null)
    }).toArray
    
    val incomings = new Array[collection.Set[Int]](sz)
    
    for (i <- 0 until sz) incomings(i) = collection.mutable.HashSet[Int]()
    for (i <- 0 until sz; linkto <- pages(i).outgoing) incomings(linkto) += i
    for (i <- 0 until sz) pages(i).incoming = incomings(i).toArray
    
    pages
  }
}


object PageRank extends Benchmark {
  val pages = Page.middlePages(sz)
  val prob1 = new Array[Double](sz)
  val prob2 = new Array[Double](sz)
  var pct: ParCtrie[Int, Page] = null
    
  println("Page set constructed.")
  
  override def setUp() {
    val in = 1.0 / sz
    for (i <- 0 until sz) prob1(i) = in
    for (i <- 0 until sz) prob2(i) = in
    pct = new ParCtrie[Int, Page](1 << 20)
    for (i <- 0 until sz) pct.put(i, pages(i))
  }
  
  def run() {
    for ((name, page) <- pct) {
      
    }
  }
}












