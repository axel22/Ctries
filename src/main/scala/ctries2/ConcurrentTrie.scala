package ctries2



import java.util.concurrent.atomic._
import annotation.tailrec
import annotation.switch



final class INode[K, V](private val updater: AtomicReferenceFieldUpdater[INodeBase, AnyRef]) extends INodeBase {
  
  import INodeBase._
  
  @inline final def CAS(old: AnyRef, n: AnyRef) = updater.compareAndSet(this, old, n)
  
  private def inode(cn: CNode[K, V]) = {
    val nin = new INode[K, V](updater)
    /*WRITE*/nin.mainnode = cn
    nin
  }
  
  final def insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V]): Boolean = {
    val m = /*READ*/mainnode
    
    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] => in.insert(k, v, hc, lev + 5, this)
            case sn: SNode[K, V] if !sn.tomb =>
              if (sn.k != k) CAS(cn, cn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc, false), hc, lev + 5))))
              else CAS(cn, cn.updatedAt(pos, new SNode(k, v, hc, false)))
            case sn: SNode[K, V] if sn.tomb => // fix!
              if (parent ne null) clean(parent)
              false
          }
        } else {
          val len = cn.array.length
          val narr = new Array[BasicNode](len + 1)
          val ncnode = new CNode[K, V](bmp | flag, narr)
          Array.copy(cn.array, 0, narr, 0, pos)
          narr(pos) = new SNode(k, v, hc, false)
          Array.copy(cn.array, pos, narr, pos + 1, len - pos)
          CAS(cn, ncnode)
        }
      case null => // 2) a null-i-node, fix and retry
        if (parent ne null) clean(parent)
        false
      case sn: SNode[K, V] if sn.tomb =>
        clean(parent)
        false
    }
  }
  
  final def lookup(k: K, hc: Int, lev: Int, parent: INode[K, V]): AnyRef = {
    val m = /*READ*/mainnode
    
    m match {
      case cn: CNode[K, V] => // 1) a multinode
        val idx = (hc >>> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        if ((bmp & flag) == 0) null // 1a) bitmap shows no binding
        else { // 1b) bitmap contains a value - descend
          val pos = Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          sub match {
            case in: INode[K, V] => in.lookup(k, hc, lev + 5, this)
            case sn: SNode[K, V] if !sn.tomb => // 2) singleton node
              if (sn.hc == hc && sn.k == k) sn.v.asInstanceOf[AnyRef]
              else null
            case sn: SNode[K, V] if sn.tomb => // fix!
              if (parent ne null) clean(parent)
              RESTART
          }
        }
      case sn: SNode[K, V] => // 3) non-live node
        clean(parent)
        RESTART
      case null  => // 4) a null-i-node
        if (parent ne null) clean(parent)
        RESTART
    }
  }
  
  final def remove(k: K, hc: Int, lev: Int, parent: INode[K, V]): Option[V] = {
    val m = /*READ*/mainnode
    
    None
  }
  
  private def clean(parent: INode[K, V]) {
    val parentm = parent.mainnode
    parentm match {
      case cn: CNode[K, V] => parent.CAS(cn, cn.toCompressed)
      case _ =>
    }
  }
  
  final def isNullInode = mainnode eq null
  
  def string(lev: Int) = "%sINode -> %s".format("  " * lev, mainnode match {
    case null => "<null>"
    case sn: SNode[_, _] => "SNode(%s, %s, %d, %c)".format(sn.k, sn.v, sn.hc, if (sn.tomb) '!' else '_')
    case cn: CNode[_, _] => cn.string(lev)
    case x => "<elem: %s>".format(x)
  })
  
}


final class SNode[K, V](final val k: K, final val v: V, final val hc: Int, final val tomb: Boolean) extends BasicNode {
  final def copy = new SNode(k, v, hc, tomb)
  final def copyTombed = new SNode(k, v, hc, true)
  final def copyUntombed = new SNode(k, v, hc, false)
  final def string(lev: Int) = ("  " * lev) + "SNode(%s, %s, %d, %c)".format(k, v, hc, if (tomb) '!' else '_')
}


final class CNode[K, V](bmp: Int, a: Array[BasicNode]) extends CNodeBase[K, V] {
  bitmap = bmp
  array = a
  
  final def updatedAt(pos: Int, nn: BasicNode) = {
    val len = array.length
    val narr = new Array[BasicNode](len)
    Array.copy(array, 0, narr, 0, len)
    narr(pos) = nn
    new CNode(bitmap, narr)
  }
  
  private def resurrect(inode: INode[K, V], inodemain: AnyRef) = inodemain match {
    case sn: SNode[_, _] if sn.tomb =>
      val newinode = new INode[K, V](ConcurrentTrie.inodeupdater)
      newinode.mainnode = sn.copyUntombed
      newinode
    case _ => inode
  }
  
  private def extractSNode(n: BasicNode) = n match {
    case sn: SNode[K, V] => sn.copyUntombed
    case in: INode[K, V] => in.mainnode match {
      case sn: SNode[K, V] => sn.copyUntombed
    }
  }
  
  private def isTombed(bn: BasicNode) = bn match {
    case in: INode[K, V] =>
      val m = /*READ*/in.mainnode
      m match {
        case sn: SNode[K, V] if sn.tomb => true
        case _ => false
      }
    case _ => false
  }
  
  final def toCompressed = {
    var bmp = bitmap
    val maxsubnodes = Integer.bitCount(bmp) // !!!this ensures lock-freedom!!!
    if (maxsubnodes == 1 && isTombed(array(0))) extractSNode(array(0)).copyUntombed
    else {
      var nbmp = 0
      var i = 0
      val arr = array
      var nsz = 0
      val tmparray = new Array[INode[K, V]](arr.length)
      while (bmp != 0) { // construct new bitmap
        val lsb = bmp & (-bmp)
        val sub = arr(i)
        sub match {
          case in: INode[K, V] =>
            val inodemain = in.mainnode
            if (inodemain ne null) {
              nbmp |= lsb
              tmparray(nsz) = resurrect(in, inodemain)
              nsz += 1
            }
          case sn =>
        }
        bmp ^= lsb
        i += 1
      }
      
      if (nsz > 0) {
        val narr = new Array[BasicNode](nsz)
        Array.copy(tmparray, 0, narr, 0, nsz)
        new CNode(nbmp, narr)
      } else null
    }
  }
  
  private[ctries2] def string(lev: Int): String = "CNode %x\n%s".format(bitmap, array.map(_.string(lev + 1)).mkString("\n"))
}


object CNode {
  def singular[K, V](k: K, v: V, hc: Int, lev: Int) = {
    val sn = new SNode(k, v, hc, false)
    val flag = 1 << ((hc >>> lev) & 0x1f)
    val arr = new Array[BasicNode](1)
    arr(0) = sn
    new CNode(flag, arr)
  }
  
  def dual[K, V](x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int): CNode[K, V] = if (lev < 35) {
    val xidx = (xhc >>> lev) & 0x1f
    val yidx = (yhc >>> lev) & 0x1f
    val bmp = (1 << xidx) | (1 << yidx)
    if (xidx == yidx) {
      val subinode = new INode[K, V](ConcurrentTrie.inodeupdater)
      subinode.mainnode = dual(x, xhc, y, yhc, lev + 5)
      new CNode(bmp, Array(subinode))
    } else {
      if (xidx < yidx) new CNode(bmp, Array(x, y))
      else new CNode(bmp, Array(y, x))
    }
  } else sys.error("list nodes not supported yet, lev=%d; %s, %s".format(lev, x.string(lev), y.string(lev)))
}


// TODO final class LNode


class ConcurrentTrie[K, V] extends ConcurrentTrieBase[K, V] {
  root = null
  private val rootupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[ConcurrentTrieBase[_, _]], classOf[INode[_, _]], "root")
  
  @inline private def computeHash(k: K): Int = {
    k.hashCode
  }
  
  final def insert(k: K, v: V) {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }
  
  @tailrec private def inserthc(k: K, hc: Int, v: V) {
    val r = /*READ*/root
    
    // 0) check if the root is a null reference - if so, allocate a new root
    if ((r eq null) || r.isNullInode) {
      val nroot = new INode[K, V](ConcurrentTrie.inodeupdater)
      nroot.mainnode = CNode.singular(k, v, hc, 0)
      if (!rootupdater.compareAndSet(this, r, nroot)) inserthc(k, hc, v)
    } else if (!r.insert(k, v, hc, 0, null)) inserthc(k, hc, v)
  }
  
  final def lookupOpt(k: K): Option[V] = {
    val hc = computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }
  
  final def lookup(k: K): V = {
    val hc = computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }
  
  @tailrec private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = /*READ*/root
    if (r eq null) null
    else {
      val res = r.lookup(k, hc, 0, null)
      if (res ne INodeBase.RESTART) res
      else {
        if (r.isNullInode) rootupdater.compareAndSet(this, r, null)
        lookuphc(k, hc)
      }
    }
  }
  
  final def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k, hc)
  }
  
  private def removehc(k: K, hc: Int): Option[V] = {
    val r = /*READ*/root
    if (r eq null) None
    else {
      val res = r.remove(k, hc, 0, null)
      if (res ne null) res
      else {
        if (r.isNullInode) rootupdater.compareAndSet(this, r, null)
        removehc(k, hc)
      }
    }
  }
  
  private[ctries2] def string = if (root != null) root.string(0) else "<null>"
  
}


object ConcurrentTrie {
  val inodeupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase], classOf[AnyRef], "mainnode")
}


object RestartException extends util.control.ControlThrowable


