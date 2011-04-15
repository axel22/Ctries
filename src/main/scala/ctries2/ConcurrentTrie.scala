package ctries2



import java.util.concurrent.atomic._
import annotation.tailrec
import annotation.switch



final class INode[K, V](private val updater: AtomicReferenceFieldUpdater[INodeBase, AnyRef]) extends INodeBase {
  
  import INodeBase._
  
  @inline final def CAS(old: AnyRef, n: AnyRef) = updater.compareAndSet(this, old, n)
  
  @tailrec final def insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V]): Boolean = {
    val m = mainnode
    
    // this node is never initially empty - it will always contain at least 1 value when created
    // if the inode contains a null, that means its contents have been removed
    m match {
      case cn: CNode[K, V] => // 2) multiple values at this node
        // 2) calculate the position in the bitmap
        val idx = (hc >> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 2a) there is a binding at the position and it's not null - descend
          cn.array(pos).insert(k, v, hc, lev + 5, this)
        } else { // 2b) no binding at the position - create a new node
          val len = cn.array.length
          val arr = new Array[INode[K, V]](len + 1)
          val ncnode = new CNode[K, V](bmp | flag, arr)
          Array.copy(cn.array, 0, arr, 0, pos)
          arr(pos) = s_inode(new SNode(k, v, hc, false))
          Array.copy(cn.array, pos, arr, pos + 1, len - pos)
          CAS(cn, ncnode)
        }
      case sn: SNode[K, V] if !sn.tomb => // 1) a singleton node - we rely on the fact that singletons below the root are never compressed
        val yk = sn.k
        val yhc = sn.hc
        // 1a) if they have the same keys, replace the old binding
        if (yhc == hc && yk == k) CAS(sn, new SNode(k, v, hc, false))
        // 1b) if they have different keys, create a new INode and put it here
        else CAS(sn, cnode(new SNode(k, v, hc, false), hc, sn.copy, yhc, lev))
      case sn: SNode[K, V] if sn.tomb => // 3) a tomb singleton - update this position through the parent
        // lemma: a tomb-inode no longer changes its value
        clean(parent)
        false
      case null if parent ne null => // 4) a deleted node - update this position through the parent
        // lemma: a null-inode no longer changes its value
        clean(parent)
        false
      case null => // 5) root null - retry from root
        false
    }
  }
  
  private def clean(parent: INode[K, V]) {
    val parentm = parent.mainnode
    parentm match {
      case cn: CNode[K, V] =>
        parent.CAS(cn, cn.toCompressed)
      case _ =>
    }
  }
  
  private def cnode(x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int): CNode[K, V] = if (lev < 35) {
    val xidx = (xhc >> lev) & 0x1f
    val yidx = (yhc >> lev) & 0x1f
    val bmp = (1 << xidx) | (1 << yidx)
    if (xidx == yidx) {
      val subinode = new INode[K, V](updater)
      subinode.mainnode = cnode(x, xhc, y, yhc, lev + 5)
      new CNode(bmp, Array(subinode))
    } else {
      if (xidx < yidx) new CNode(bmp, Array(s_inode(x), s_inode(y)))
      else new CNode(bmp, Array(s_inode(y), s_inode(x)))
    }
  } else sys.error("list nodes not supported yet")
  
  private def s_inode(sn: SNode[K, V]) = {
    val in = new INode[K, V](updater)
    in.mainnode = sn
    in
  }
  
  @tailrec
  final def lookup(k: K, hc: Int, lev: Int, m: AnyRef, parent: INode[K, V]): AnyRef = {
    m match {
      case cn: CNode[K, V] => // 2) a multinode
        val idx = (hc >> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        if ((bmp & flag) == 0) null // 2a) bitmap shows no binding
        else { // 2b) bitmap contains a value - descend
          val pos = Integer.bitCount(bmp & (flag - 1))
          val subinode = cn.array(pos)
          subinode.lookup(k, hc, lev + 5, subinode.mainnode, this)
        }
      case sn: SNode[K, V] if !sn.tomb => // 1) singleton node
        if (sn.hc == hc && sn.k == k) sn.v.asInstanceOf[AnyRef]
        else null
      case sn: SNode[K, V] => // 3) non-live node
        clean(parent)
        throw RestartException
      case null if parent ne null =>
        clean(parent)
        throw RestartException
      case null => null
    }
  }
  
  final def remove(k: K, hc: Int, lev: Int, parent: INode[K, V]): Option[V] = {
    var m = mainnode
    
    m match {
      case sn: SNode[K, V] if !sn.tomb => // 1) singleton node
        if (sn.hc == hc && sn.k == k) {
          if (CAS(sn, null)) Some(sn.v) else null
        } else None
      case cn: CNode[K, V] => // 2) a multinode
        val idx = (hc >> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        if ((bmp & flag) == 0) None // 2a) binding not found
        else { // 2b) there is a binding - go lower
          val pos = Integer.bitCount(bmp & (flag - 1))
          val res = cn.array(pos).remove(k, hc, lev + 5, this)
          
          // 2b) start compression
          res match {
            case null => null // restarting
            case None => None // no changes, since nothing was found
            case _ =>
              // do a contraction until you succeed or this becomes a non-multiway
              @tailrec def contract(): Unit = {
                mainnode match {
                  case old: CNode[K, V] => if (!CAS(old, old.toContracted)) contract()
                  case _ =>
                }
              }
              
              // if this is possibly a single tip, do a tomb markup
              @tailrec def tombmarkup(): Int = {
                mainnode match {
                  case old: CNode[K, V] =>
                    val r = old.tryTombSingleton()
                    if (r == 0) tombmarkup() else r
                  case _ => -2
                }
              }
              
              // OLD VERSION
              // tombmarkup()
              // contract()
              // res
              
              // TODO optimize this
              // finds a singleton and tries to tomb it
              // -2 - if this isn't a cnode at all
              // -1 - if there are only null-inodes below
              //  1 - if tomb succeeded
              //  2 - a singleton, and already tombed
              //  3 - if there is no single singleton, and no null-inodes below
              //  4 - if there is no single singleton, but there are null-inodes below
              (tombmarkup(): @switch) match {
                case -2 =>
                  res // retry contract higher up
                case -1 =>
                  contract()
                  res
                case 1 =>
                  contract()
                  res
                case 2 =>
                  contract()
                  res
                case 3 =>
                  res // could avoid retrying contract with new Result(res), but it doesn't get much better
                case 4 =>
                  contract()
                  res
                case _ => sys.error("unreachable")
              }
          }
        }
      case sn: SNode[K, V] if sn.tomb => // 3) tomb node - compress parent and restart
        if (sn.hc == hc && sn.k == k) { // we've found a candidate
          val parentm = parent.mainnode
          parentm match {
            case cn: CNode[K, V] =>
              // try to reach sn once more from the parent
              val idx = (hc >> (lev - 5)) & 0x1f
              val flag = 1 << idx
              val bmp = cn.bitmap
              val pos = Integer.bitCount(bmp & (flag - 1))
              if ((bmp & flag) != 0 && cn.array(pos) == this) { // tomb is still reachable - try to rewrite the parent without this node
                val arr = cn.array
                val nlen = arr.length - 1
                val nbmp = bmp ^ flag
                val narr = new Array[INode[K, V]](nlen)
                Array.copy(arr, 0, narr, 0, pos)
                Array.copy(arr, pos + 1, narr, pos, nlen - pos)
                val ncnode = new CNode(nbmp, narr)
                if (parent.CAS(cn, ncnode)) Some(sn.v) // rewritten the parent - success!
                else null // a modification on the parent - restart
              } else null // a modification on the parent - restart
            case _ => null // a modification on the parent - restart
          }
        } else None // we've found no such binding
      case null if parent ne null =>
        clean(parent)
        null // restart
      case null => None // 5) root null node, no need to recompress, but restart
    }
  }
  
  // does this inode hold a tombed singleton
  // if true, guaranteed to stay that way
  @inline final def isTombed = {
    val m = mainnode
    m match {
      case sn: SNode[_, _] if sn.tomb => true
      case _ => false
    }
  }
  
  @inline final def isLive = {
    val m = mainnode
    m match {
      case sn: SNode[_, _] if sn.tomb => true
      case null => true
      case _ => false
    }
  }
  
  @inline final def nonLive = !isLive
  
  @inline final def isSingleton = {
    val m = mainnode
    m match {
      case sn: SNode[_, _] => true
      case _ => false
    }
  }
  
  private[ctries2] def string(lev: Int) = "%sINode -> %s".format("  " * lev, mainnode match {
    case null => "<null>"
    case sn: SNode[_, _] => "SNode(%s, %s, %d, %c)".format(sn.k, sn.v, sn.hc, if (sn.tomb) '!' else '_')
    case cn: CNode[_, _] => cn.string(lev)
  })
  
}


final class CNode[K, V](bmp: Int, a: Array[INode[K, V]]) extends CNodeBase[K, V] {
  assert(Integer.bitCount(bmp) == a.length) // TODO comment this line
  bitmap = bmp
  array = a
  
  private[ctries2] def string(lev: Int): String = "CNode %x\n%s".format(bitmap, array.map(_.string(lev + 1)).mkString("\n"))
  
  private def resurrect(inode: INode[K, V], inodemain: AnyRef) = inodemain match {
    case sn: SNode[_, _] if sn.tomb =>
      val newinode = new INode[K, V](ConcurrentTrie.inodeupdater)
      newinode.mainnode = sn.copyUntombed
      newinode
    case _ => inode
  }
  
  @inline private def isSNode(n: AnyRef) = (n ne null) && n.isInstanceOf[SNode[_, _]]

  @inline private def asSNode(n: AnyRef) = n.asInstanceOf[SNode[K, V]]
  
  // finds a singleton and tries to tomb it
  // -1 - if there are only null-inodes below
  //  0 - if singleton was found and tomb failed
  //  1 - if tomb succeeded
  //  2 - a singleton, and already tombed
  //  3 - if there is no single singleton, and no null-inodes below
  //  4 - if there is no single singleton, but there are null-inodes below
  final def tryTombSingleton(): Int = {
    val arr = array
    val len = arr.length
    var totalnulls = 0
    var i = 0
    var x: INode[K, V] = null
    var m: AnyRef = null
    while (i < len) {
      val inode = arr(i)
      val main = inode.mainnode
      if (main ne null) { // found a non-null-inode
        if (x eq null) { // found first non-null-inode below
          x = inode
          m = main
        } else return 3 // found more than a single one
      } else totalnulls += 1 // found a null-inode
      i += 1
    }
    if (x ne null) m match {
      case sn: SNode[K, V] if !sn.tomb => if (x.CAS(sn, sn.copyTombed)) 1 else 0
      case sn: SNode[K, V] => 2
      case _ => if (totalnulls == 0) 3 else 4
    } else -1
  }
  
  // - will remove at least the null-inodes and tomb-inodes present at the beginning
  //   and possibly some that appear during the compression
  // - if it detects a singleton
  final def toCompressed = {
    var bmp = bitmap
    val maxsubnodes = Integer.bitCount(bmp) // !!!this ensures lock-freedom!!!
    if (maxsubnodes == 1 && array(0).isTombed) asSNode(array(0).mainnode).copyUntombed
    else {
      var nbmp = 0
      var i = 0
      val arr = array
      var nsz = 0
      val tmparray = new Array[INode[K, V]](arr.length)
      while (bmp != 0) { // construct new bitmap
        val lsb = bmp & (-bmp)
        val inode = arr(i)
        val inodemain = inode.mainnode
        if (inodemain ne null) {
          nbmp |= lsb
          tmparray(nsz) = resurrect(inode, inodemain)
          nsz += 1
        }
        bmp ^= lsb
        i += 1
      }
      
      if (nsz > 0) {
        val narr = new Array[INode[K, V]](nsz)
        Array.copy(tmparray, 0, narr, 0, nsz)
        new CNode(nbmp, narr)
      } else null
    }
  }
  
  // - will remove at least the null-inodes present at the beginning
  //   and possibly some that appear during the compression
  // - will not resurrect tomb singletons
  // - if this cnode is part of a tomb-tip, it will contract the node, shortening the branch
  final def toContracted = {
    val arr = array
    val len = array.length
    var i = 0
    var bmp = bitmap
    val tmparray = new Array[INode[K, V]](arr.length)
    var nsz = 0
    var nbmp = 0
    while (bmp != 0) { // construct new bitmap
      val lsb = bmp & (-bmp)
      val inode = arr(i)
      val inodemain = inode.mainnode
      if (inodemain ne null) {
        nbmp |= lsb
        tmparray(nsz) = inode
        nsz += 1
      }
      bmp ^= lsb
      i += 1
    }
    
    if (nsz == 1 && tmparray(0).isTombed) {
      asSNode(tmparray(0).mainnode).copyUntombed
    } else if (nsz > 0) {
      val narr = new Array[INode[K, V]](nsz)
      Array.copy(tmparray, 0, narr, 0, nsz)
      new CNode(nbmp, narr)
    } else null
  }
}


final class SNode[K, V](final val k: K, final val v: V, final val hc: Int, final val tomb: Boolean) {
  final def copy = new SNode(k, v, hc, tomb)
  final def copyTombed = new SNode(k, v, hc, true)
  final def copyUntombed = new SNode(k, v, hc, false)
}


// TODO final class LNode


class ConcurrentTrie[K, V] extends ConcurrentTrieBase[K, V] {
  root = new INode[K, V](ConcurrentTrie.inodeupdater)
  private val rootupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[ConcurrentTrieBase[_, _]], classOf[INode[_, _]], "root")
  
  @inline private def computeHash(k: K): Int = {
    k.hashCode
  }
  
  final def insert(k: K, v: V) {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }
  
  @tailrec private def inserthc(k: K, hc: Int, v: V) {
    val r = root
    
    // 0) check if the root node contains a null reference - if so, allocate a new root
    if (r.mainnode eq null) {
      val nroot = new INode[K, V](ConcurrentTrie.inodeupdater)
      nroot.mainnode = new SNode(k, v, hc, false)
      if (!rootupdater.compareAndSet(this, r, nroot)) inserthc(k, hc, v)
    } else if (!root.insert(k, v, hc, 0, null)) inserthc(k, hc, v)
  }
  
  final def lookup(k: K): V = {
    val hc = computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }
  
  //@tailrec
  private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = root
    val rm = r.mainnode
    try {
      r.lookup(k, hc, 0, rm, null)
    } catch {
      case RestartException => lookuphc(k, hc)
    }
  }

  final def lookupOpt(k: K): Option[V] = {
    val hc = computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }
  
  final def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k, hc)
  }
  
  @tailrec private def removehc(k: K, hc: Int): Option[V] = {
    val r = root
    
    // 0) check if the root node contains a null reference
    if (r.mainnode eq null) None
    else {
      val res = r.remove(k, hc, 0, null)
      if (res eq null) removehc(k, hc) else res
    }
  }
  
  private[ctries2] def string = root.string(0)
  
}


object ConcurrentTrie {
  val inodeupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase], classOf[AnyRef], "mainnode")
}


object RestartException extends util.control.ControlThrowable

