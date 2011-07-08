package ctries



import Global._



object Length {
  import ctries2.ConcurrentTrie
  
  val ct = new ConcurrentTrie[Elem, Elem]
  for (i <- 0 until sz) ct.put(elems(i), elems(i))
  
  def run() {
    
  }
  
}
