package ctries






case class Elem(i: Int) extends Comparable[Elem] {
  def compareTo(y: Elem) = i - y.i

  override def hashCode = {
    // var hc = i * 0x9e3775cd
    // hc = java.lang.Integer.reverseBytes(hc)
    // hc * 0x9e3775cd
    
    var hc = i * 0x9e3775cd
    // hc + (hc << 16) + i
    hc
  }
}


object Global {
  val sz = System.getProperty("sz").toInt
  val par = Option(System.getProperty("par")).map(_.toInt)
  val elems = (for (i <- 0 until (sz * 2)) yield Elem(i)).toArray
}


