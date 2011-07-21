package ctries



import Global._



object Lengths {
  import ctries2._
  
  val ct = new ConcurrentTrie[Elem, Elem]
  for (i <- 0 until sz) ct.put(elems(i), elems(i))
  
  def maxdepth[K, V](ct: ConcurrentTrie[K, V], in: INode[K, V], acc: Int): Int = {
    in.GCAS_READ(ct) match {
      case cn: CNode[K, V] => (for (elem <- cn.array) yield elem match {
        case sin: INode[K, V] => maxdepth(ct, sin, acc + 1)
        case sn: SNode[K, V] => acc + 1
      }).max
      case tn: TNode[K, V] => acc + 1
      case ln: LNode[K, V] => acc + 1
      case null => acc + 1
    }
  }
  
  def avgdepth[K, V](ct: ConcurrentTrie[K, V], in: INode[K, V], d: Long): (Int, Long) = {
    in.GCAS_READ(ct) match {
      case cn: CNode[K, V] => (for (elem <- cn.array) yield elem match {
        case sin: INode[K, V] => avgdepth(ct, sin, d + 1)
        case sn: SNode[K, V] => (1, d)
      }).reduceLeft((a, b) => (a._1 + b._1, a._2 + b._2))
      case tn: TNode[K, V] => (1, d)
      case ln: LNode[K, V] => (1, d)
      case null => (1, d)
    }
  }
  
  def avgnodes[K, V](ct: ConcurrentTrie[K, V], in: INode[K, V], d: Long): (Int, Long) = {
    in.GCAS_READ(ct) match {
      case cn: CNode[K, V] => (for (elem <- cn.array) yield elem match {
        case sin: INode[K, V] => avgnodes(ct, sin, d + 2)
        case sn: SNode[K, V] => (1, d + 2)
      }).reduceLeft((a, b) => (a._1 + b._1, a._2 + b._2))
      case tn: TNode[K, V] => (1, d + 1)
      case ln: LNode[K, V] => (1, d + 1)
      case null => (1, d + 1)
    }
  }
  
  def histogram[K, V](ct: ConcurrentTrie[K, V], in: INode[K, V], d: Int, hist: Array[Long]) {
    in.GCAS_READ(ct) match {
      case cn: CNode[K, V] => for (elem <- cn.array) elem match {
        case sin: INode[K, V] => histogram(ct, sin, d + 2, hist)
        case sn: SNode[K, V] => hist(d + 2) += 1
      }
      case tn: TNode[K, V] => hist(d + 1) += 1
      case ln: LNode[K, V] => hist(d + 1) += 1
      case null => hist(d + 1) += 1
    }
  }
  
  val HSZ = 24
  val HGT = 40
  
  def printHist(hist: Array[Long]) {
    val max = hist.max
    val bars = for (i <- 0 until HSZ) yield {
      val hgt = (1.0 * hist(i) / max * HGT).toInt
      (Seq.fill(HGT - hgt)("    ")) ++
      (Seq.fill(hgt)("  * ")) ++
      (Seq("|" + (" " * (2 - i.toString.length)) + i + " "))
    }
    val lines = bars.transpose
    val decorlines = lines.reverse.zipWithIndex.map {
      case (line, i) =>
        val num = (1.0 * i / HGT * max).toInt.toString
        Seq(" " * (9 - num.length) + num + " |") ++ line
    }.reverse
    for (line <- decorlines) {
      for (elem <- line) print(elem)
      println()
    }
  }
  
  def main(args: Array[String]) {
    val maxd = maxdepth(ct, ct.RDCSS_READ_ROOT(), 1)
    println("Maximum inode: %d".format(maxd))
    
    val (sntotal, snpath) = avgdepth(ct, ct.RDCSS_READ_ROOT(), 1L)
    println("Average inode: %f = %d / %d".format(1.0 * snpath / sntotal, snpath, sntotal))
    
    val (ndtotal, ndpath) = avgnodes(ct, ct.RDCSS_READ_ROOT(), 1L)
    println("Average nodes: %f = %d / %d".format(1.0 * ndpath / ndtotal, ndpath, ndtotal))
    
    val hist = new Array[Long](HSZ)
    histogram(ct, ct.RDCSS_READ_ROOT(), 1, hist)
    printHist(hist)
    
    if (sz < 128) println(ct.string)
  }
  
}
