package ctries



import Global._
import scala.testing.Benchmark
import scala.collection.parallel._



// a parallel ctrie created using the parallel collections framework
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
  
  def calcIncoming(pages: Array[Page]) {
    val sz = pages.length
    val incomings = new Array[collection.Set[Int]](sz)
    
    for (i <- 0 until sz) incomings(i) = collection.mutable.HashSet[Int]()
    for (i <- 0 until sz; linkto <- pages(i).outgoing) incomings(linkto) += i
    for (i <- 0 until sz) pages(i).incoming = incomings(i).toArray.filter(_ != i)
  }
  
  // creates a set of pages with most links to initial pages
  def initialGauss(sz: Int) = {
    val pages = (for (i <- 0 until sz) yield {
      val linknum = rand.nextInt(maxlinks.get)
      val outgoing = for (j <- 0 until linknum) yield {
        val lnk = (math.abs(rand.nextGaussian() / 4) * sz) max 0 min (sz - 1)
        lnk.toInt
      }
      Page(i, linknum, outgoing.toArray, null)
    }).toArray
    
    calcIncoming(pages)
    
    pages
  }
  
  def groups(sz: Int) = {
    val pages = (for (i <- 0 until sz) yield {
      val linknum = rand.nextInt(maxlinks.get)
      val outgoing = for (j <- 0 until linknum) yield {
        val lnk = (rand.nextGaussian() * maxlinks.get + i) max 0 min (sz - 1)
        lnk.toInt
      }
      Page(i, linknum, outgoing.toArray, null)
    }) toArray
    
    calcIncoming(pages)
    
    pages
  }
  
  def nonUniform(sz: Int) = {
    val pages = (for (i <- 0 until sz) yield {
      val linknum = rand.nextInt(maxlinks.get)
      val group = (i / maxlinks.get) * maxlinks.get
      val linksin = for (j <- 0 until linknum) yield {
        val lnk = (rand.nextGaussian() * maxlinks.get + group) max 0 min (sz - 1)
        lnk.toInt
      }
      val linksprev = for (j <- 0 until linknum) yield {
        val lnk = (rand.nextGaussian() * maxlinks.get + group - maxlinks.get) max 0 min (sz - 1)
        lnk.toInt
      }
      val linkshalf = for (j <- 0 until linknum) yield {
        val lnk = (rand.nextGaussian() * maxlinks.get + group / 2) max 0 min (sz - 1)
        lnk.toInt
      }
      Page(i, linknum, (linksin ++ linksprev ++ linkshalf).toArray, null)
    }) toArray
    
    calcIncoming(pages)
    
    pages
  }
  
  def pages(sz: Int) = pagegenerator.get match {
    case "gauss-init" => initialGauss(sz)
    case "groups" => groups(sz)
    case "non-uniform" => nonUniform(sz)
  }
  
}


object SeqPageRank extends Benchmark {
  import ctries2.{ConcurrentTrie => Ctrie}
  
  val pages = Page.pages(sz)
  val prob1 = new Array[Double](sz)
  val prob2 = new Array[Double](sz)
  var ct: Ctrie[Int, Page] = null
  
  if (debug) println("Page set constructed.")
  
  override def setUp() {
    val in = 1.0 / sz
    for (i <- 0 until sz) prob1(i) = in
    for (i <- 0 until sz) prob2(i) = in
    ct = new Ctrie[Int, Page]
    for (i <- 0 until sz) ct.put(i, pages(i))
  }
  
  def run() {
    val d = damping.get
    val epsilon = d / sz / 1000
    var iter = 0
    while (ct.nonEmpty) {
      val (last, next) = if (iter % 2 == 0) (prob1, prob2) else (prob2, prob1)
      for ((name, page) <- ct) {
        var sum = 0.0
        for (pind <- page.incoming) sum += last(pind) / pages(pind).linksnum
        next(name) = (1 - d) / sz + d * sum
        if (next(name) - last(name) < epsilon) ct.remove(name)
      }
      iter += 1
      
      if (debug) if (iter % 1 == 0) println("Iteration %d, size %d".format(iter, ct.size))
    }
    
    if (debug) println("No. iterations: " + iter)
  }
}


object ParPageRank extends Benchmark {
  val pages = Page.pages(sz)
  val prob1 = new Array[Double](sz)
  val prob2 = new Array[Double](sz)
  var pct: ParCtrie[Int, Page] = null
  
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par.get)
  
  if (debug) println("Page set constructed.")
  
  override def setUp() {
    val in = 1.0 / sz
    for (i <- 0 until sz) prob1(i) = in
    for (i <- 0 until sz) prob2(i) = in
    pct = new ParCtrie[Int, Page](1 << 20)
    for (i <- 0 until sz) pct.put(i, pages(i))
  }
  
  def run() {
    val d = damping.get
    val epsilon = d / sz / 1000
    var iter = 0
    while (pct.seq.nonEmpty) {
      val (last, next) = if (iter % 2 == 0) (prob1, prob2) else (prob2, prob1)
      for ((name, page) <- pct) {
        var sum = 0.0
        for (pind <- page.incoming) sum += last(pind) / pages(pind).linksnum
        next(name) = (1 - d) / sz + d * sum
        if (next(name) - last(name) < epsilon) pct.remove(name)
      }
      iter += 1
      
      if (debug) if (iter % 10 == 0) println("Iteration %d, size %d".format(iter, pct.seq.size))
    }
    
    if (debug) println("No. iterations: " + iter)
  }
}


object FilterPageRank extends Benchmark {
  val pages = Page.pages(sz)
  val prob1 = new Array[Double](sz)
  val prob2 = new Array[Double](sz)
  val initialpages = pages.map(p => (p.name, p)).toMap
  
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par.get)
  
  if (debug) println("Page set constructed.")
  
  override def setUp() {
    val in = 1.0 / sz
    for (i <- 0 until sz) prob1(i) = in
    for (i <- 0 until sz) prob2(i) = in
  }
  
  def run() {
    val d = damping.get
    val epsilon = d / sz / 1000
    var remainingpages = initialpages.par
    var iter = 0
    while (remainingpages.nonEmpty) {
      val (last, next) = if (iter % 2 == 0) (prob1, prob2) else (prob2, prob1)
      for ((name, page) <- remainingpages) {
        var sum = 0.0
        for (pind <- page.incoming) sum += last(pind) / pages(pind).linksnum
        next(name) = (1 - d) / sz + d * sum
      }
      remainingpages = remainingpages.filter(kv => next(kv._1) - last(kv._1) >= epsilon)
      iter += 1
      
      if (debug) if (iter % 10 == 0) println("Iteration %d, size %d".format(iter, remainingpages.size))
    }
    
    if (debug) println("No. iterations: " + iter)
  }
  
}








