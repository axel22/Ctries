package ctries2



import java.util.concurrent.atomic._
import collection.mutable.ConcurrentMap
import collection.immutable.ListMap
import annotation.tailrec
import annotation.switch



final class INode[K, V](g: Int) extends INodeBase(g) {
  
  import INodeBase._
  
  @inline final def CAS(old: BasicNode, n: BasicNode) = INodeBase.updater.compareAndSet(this, old, n)
  
  @inline final def GCAS_READ(ct: ConcurrentTrie[K, V]): BasicNode = {
    val m = /*READ*/mainnode
    val prevval = /*READ*/m.prev
    if ((m eq null) || (prevval eq null)) m else GCAS_COMPLETE(m, ct)
  }
  
  @tailrec final def GCAS_COMPLETE(m: BasicNode, ct: ConcurrentTrie[K, V]): BasicNode = {
    // complete the GCAS
    val prev = /*READ*/m.prev
    val ctr = /*READ*/ct.root
    
    prev match {
      case null =>
        m
      case fn: FailedNode => // try to commit to previous value
        if (CAS(m, fn.prev)) fn.prev
        else GCAS_COMPLETE(/*READ*/mainnode, ct)
      case bn: BasicNode =>
        // Assume that you've read the root from the generation G.
        // Assume that the snapshot algorithm is correct.
        // ==> you can only reach nodes in generations <= G.
        // ==> `gen` is <= G.
        // We know that `ctr.gen` is >= G.
        // ==> if `ctr.gen` == `gen` then they are both equal to G.
        // ==> otherwise, we know that either `ctr.gen` > G, `gen` < G,
        //     or both
        if (ctr.gen == gen) {
          // try to commit
          if (m.CAS_PREV(prev, null)) m
          else GCAS_COMPLETE(/*READ*/m, ct)
        } else {
          // try to abort
          m.CAS_PREV(prev, new FailedNode(prev))
          GCAS_COMPLETE(/*READ*/mainnode, ct)
        }
    }
  }
  
  @inline final def GCAS(old: BasicNode, n: BasicNode, ct: ConcurrentTrie[K, V]): Boolean = {
    if (CAS(old, n)) GCAS_COMPLETE(n, ct) eq n
    else false
  }
  
  @inline private def inode(cn: BasicNode) = {
    val nin = new INode[K, V](gen)
    /*WRITE*/nin.mainnode = cn
    nin
  }
  
  @tailrec final def fast_insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Int, ct: ConcurrentTrie[K, V]): Boolean = {
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
            case in: INode[K, V] => in.fast_insert(k, v, hc, lev + 5, this, startgen, ct)
            case sn: SNode[K, V] if !sn.tomb =>
              if (sn.hc == hc && sn.k == k) CAS(cn, cn.updatedAt(pos, new SNode(k, v, hc, false)))
              else CAS(cn, cn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc, false), hc, lev + 5, gen))))
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
      case sn: SNode[K, V] =>
        //assert(sn.tomb)
        clean(parent)
        false
      case null => // 2) a null-i-node, fix and retry
        if (parent ne null) clean(parent)
        false
      case ln: LNode[K, V] => // 3) an l-node
        val nn = ln.inserted(k, v)
        CAS(ln, nn)
    }
  }
  
  @tailrec final def fast_insertif(k: K, v: V, hc: Int, cond: AnyRef, lev: Int, parent: INode[K, V], startgen: Int, ct: ConcurrentTrie[K, V]): Option[V] = {
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
            case in: INode[K, V] => in.fast_insertif(k, v, hc, cond, lev + 5, this, startgen, ct)
            case sn: SNode[K, V] if !sn.tomb => cond match {
              case null =>
                if (sn.hc == hc && sn.k == k) {
                  if (CAS(cn, cn.updatedAt(pos, new SNode(k, v, hc, false)))) Some(sn.v) else null
                } else
                  if (CAS(cn, cn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc, false), hc, lev + 5, gen))))) None else null
              case INode.KEY_ABSENT =>
                if (sn.hc == hc && sn.k == k) Some(sn.v)
                else
                  if (CAS(cn, cn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc, false), hc, lev + 5, gen))))) None else null
              case INode.KEY_PRESENT =>
                if (sn.hc == hc && sn.k == k) {
                  if (CAS(cn, cn.updatedAt(pos, new SNode(k, v, hc, false)))) Some(sn.v) else null
                } else None
              case otherv: V =>
                if (sn.hc == hc && sn.k == k && sn.v == otherv) {
                  if (CAS(cn, cn.updatedAt(pos, new SNode(k, v, hc, false)))) Some(sn.v) else null
                } else None
            }
          }
        } else cond match {
          case null | INode.KEY_ABSENT =>
            val len = cn.array.length
            val narr = new Array[BasicNode](len + 1)
            val ncnode = new CNode[K, V](bmp | flag, narr)
            Array.copy(cn.array, 0, narr, 0, pos)
            narr(pos) = new SNode(k, v, hc, false)
            Array.copy(cn.array, pos, narr, pos + 1, len - pos)
            if (CAS(cn, ncnode)) None else null
          case INode.KEY_PRESENT => None
          case otherv: V => None
        }
      case sn: SNode[K, V] =>
        clean(parent)
        null
      case null => // 2) a null-i-node, fix and retry
        if (parent ne null) clean(parent)
        null
      case ln: LNode[K, V] => // 3) an l-node
        @inline def insertln() = {
          val nn = ln.inserted(k, v)
          CAS(ln, nn)
        }
        cond match {
          case null =>
            val optv = ln.get(k)
            if (insertln()) optv else null
          case INode.KEY_ABSENT =>
            ln.get(k) match {
              case None => if (insertln()) None else null
              case optv => optv
            }
          case INode.KEY_PRESENT =>
            ln.get(k) match {
              case Some(v0) => if (insertln()) Some(v0) else null
              case None => None
            }
          case otherv: V =>
            ln.get(k) match {
              case Some(v0) if v0 == otherv => if (insertln()) Some(otherv) else null
              case _ => None
            }
        }
    }
  }
  
  @tailrec final def fast_lookup(k: K, hc: Int, lev: Int, parent: INode[K, V], startgen: Int, ct: ConcurrentTrie[K, V]): AnyRef = {
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
            case in: INode[K, V] => in.fast_lookup(k, hc, lev + 5, this, startgen, ct)
            case sn: SNode[K, V] => // 2) singleton node
              //assert(!sn.tomb)
              if (sn.hc == hc && sn.k == k) sn.v.asInstanceOf[AnyRef]
              else null
          }
        }
      case sn: SNode[K, V] => // 3) non-live node
        //assert(sn.tomb)
        clean(parent)
        //RESTART
        throw RestartException
      case null  => // 4) a null-i-node
        if (parent ne null) clean(parent)
        //RESTART
        throw RestartException
      case ln: LNode[K, V] =>
        ln.get(k).asInstanceOf[Option[AnyRef]].orNull
    }
  }
  
  final def fast_remove(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Int, ct: ConcurrentTrie[K, V]): Option[V] = {
    val m = /*READ*/mainnode
    
    m match {
      case cn: CNode[K, V] =>
        val idx = (hc >>> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        if ((bmp & flag) == 0) None
        else {
          val pos = Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          val res = sub match {
            case in: INode[K, V] => in.fast_remove(k, v, hc, lev + 5, this, startgen, ct)
            case sn: SNode[K, V] =>
              //assert(!sn.tomb)
              if (sn.hc == hc && sn.k == k && (v == null || sn.v == v)) {
                var ncn: CNode[K, V] = null
                if (cn.array.length > 1) ncn = cn.removedAt(pos, flag)
                if (CAS(cn, ncn)) Some(sn.v) else null
              } else None
          }
          
          if (res == None || (res eq null)) res
          else {
            // tomb-compress
            @tailrec def tombCompress(): Boolean = {
              val m = /*READ*/mainnode
              m match {
                case cn: CNode[K, V] =>
                  val tcn: BasicNode = cn.toWeakTombedCompressed
                  if (tcn eq cn) false // we're done, no further compression needed
                  else if (CAS(cn, tcn)) tcn match {
                    case null => true // parent contraction needed
                    case sn: SNode[K, V] => true // parent contraction needed
                    case _ => false // nothing to contract, we're done
                  } else tombCompress()
                case _ => false // we're done, no further compression needed
              }
            }
            
            @tailrec def contractParent(nonlive: AnyRef) {
              val pm = /*READ*/parent.mainnode
              pm match {
                case cn: CNode[K, V] =>
                  val idx = (hc >>> (lev - 5)) & 0x1f
                  val bmp = cn.bitmap
                  val flag = 1 << idx
                  if ((bmp & flag) == 0) {} // somebody already removed this i-node, we're done
                  else {
                    val pos = Integer.bitCount(bmp & (flag - 1))
                    val sub = cn.array(pos)
                    if (sub eq this)  nonlive match {
                      case null => if (!parent.CAS(cn, cn.removedAt(pos, flag))) contractParent(nonlive)
                      case sn: SNode[K, V] => if (!parent.CAS(cn, cn.updatedAt(pos, sn.copyUntombed))) contractParent(nonlive)
                    }
                  }
                case _ => // parent is no longer a cnode, we're done
              }
            }
            
            if (parent ne null) { // never tomb at root
              if (tombCompress()) contractParent(/*READ*/mainnode) // note: this inode is non-live in the 'if' body
            } //else clean(this) // clean root
            
            res
          }
        }
      case sn: SNode[K, V] =>
        //assert(sn.tomb)
        clean(parent)
        null
      case null =>
        if (parent ne null) clean(parent)
        null
      case ln: LNode[K, V] =>
        if (v == null) {
          val optv = ln.get(k)
          val nn = ln.removed(k)
          if (CAS(ln, nn)) optv else null
        } else ln.get(k) match {
          case optv @ Some(v0) if v0 == v =>
            val nn = ln.removed(k)
            if (CAS(ln, nn)) optv else null
          case _ => None
        }
    }
  }
  
  private def clean(nd: INode[K, V]) {
    val m = nd.mainnode
    m match {
      case cn: CNode[K, V] => nd.CAS(cn, cn.toCompressed)
      case _ =>
    }
  }
  
  final def isNullInode = mainnode eq null
  
  def string(lev: Int) = "%sINode -> %s".format("  " * lev, mainnode match {
    case null => "<null>"
    case sn: SNode[_, _] => "SNode(%s, %s, %d, %c)".format(sn.k, sn.v, sn.hc, if (sn.tomb) '!' else '_')
    case cn: CNode[_, _] => cn.string(lev)
    case ln: LNode[_, _] => ln.string(lev)
    case x => "<elem: %s>".format(x)
  })
  
}


object INode {
  val KEY_PRESENT = new AnyRef
  val KEY_ABSENT = new AnyRef
}


final class FailedNode(p: BasicNode) extends BasicNode {
  prev = p
  
  def string(lev: Int) = throw new UnsupportedOperationException
  def casPrev(ov: BasicNode, nv: BasicNode) = throw new UnsupportedOperationException
}


final class SNode[K, V](final val k: K, final val v: V, final val hc: Int, final val tomb: Boolean)
extends BasicNode {
  final def copy = new SNode(k, v, hc, tomb)
  final def copyTombed = new SNode(k, v, hc, true)
  final def copyUntombed = new SNode(k, v, hc, false)
  final def string(lev: Int) = ("  " * lev) + "SNode(%s, %s, %d, %c)".format(k, v, hc, if (tomb) '!' else '_')
}


final class LNode[K, V](final val listmap: ListMap[K, V])
extends BasicNode {
  def this(k: K, v: V) = this(ListMap(k -> v))
  def this(k1: K, v1: V, k2: K, v2: V) = this(ListMap(k1 -> v1, k2 -> v2))
  def inserted(k: K, v: V) = new LNode(listmap + ((k, v)))
  def removed(k: K) = {
    val updmap = listmap - k
    if (updmap.size > 1) new LNode(updmap)
    else {
      val (k, v) = updmap.iterator.next
      new SNode(k, v, k.hashCode, true) // create it tombed so that it gets compressed on subsequent accesses
    }
  }
  def get(k: K) = listmap.get(k)
  def string(lev: Int) = (" " * lev) + "LNode(%s)".format(listmap.mkString(", "))
}


final class CNode[K, V](bmp0: Int, a0: Array[BasicNode]) extends CNodeBase[K, V] {
  bitmap = bmp0
  array = a0
  
  final def updatedAt(pos: Int, nn: BasicNode) = {
    val len = array.length
    val narr = new Array[BasicNode](len)
    Array.copy(array, 0, narr, 0, len)
    narr(pos) = nn
    new CNode[K, V](bitmap, narr)
  }
  
  final def removedAt(pos: Int, flag: Int) = {
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len - 1)
    Array.copy(arr, 0, narr, 0, pos)
    Array.copy(arr, pos + 1, narr, pos, len - pos - 1)
    new CNode[K, V](bitmap ^ flag, narr)
  }
  
  private def resurrect(inode: INode[K, V], inodemain: AnyRef) = inodemain match {
    case sn: SNode[_, _] if sn.tomb => sn.copyUntombed
    case _ => inode
  }
  
  private def extractSNode(n: BasicNode) = n match {
    case sn: SNode[K, V] => sn
    case in: INode[K, V] => in.mainnode match {
      case sn: SNode[K, V] => sn
    }
  }
  
  private def isTombed(bn: BasicNode) = bn match {
    case in: INode[K, V] => in.mainnode match {
      case sn: SNode[K, V] if sn.tomb => true
      case _ => false
    }
    case _ => false
  }
  
  private def isSingleton(bn: BasicNode) = bn match {
    case in: INode[K, V] => in.mainnode match {
      case sn: SNode[K, V] if sn.tomb => true
      case _ => false
    }
    case sn: SNode[K, V] => true
    case _ => false
  }
  
  // - if the branching factor is 1 for this CNode, and the child
  //   is a tombed SNode, returns its tombed version
  // - otherwise, if there is at least one non-null node below,
  //   returns the version of this node with at least some null-inodes
  //   removed (those existing when the op began)
  // - if there are only null-i-nodes below, returns null
  final def toCompressed = {
    var bmp = bitmap
    val maxsubnodes = Integer.bitCount(bmp) // !!!this ensures lock-freedom!!!
    if (maxsubnodes == 1 && isTombed(array(0))) extractSNode(array(0)).copyTombed
    else {
      var nbmp = 0
      var i = 0
      val arr = array
      var nsz = 0
      val tmparray = new Array[BasicNode](arr.length)
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
          case sn: SNode[K, V] =>
            //assert(!sn.tomb)
            nbmp |= lsb
            tmparray(nsz) = sn
            nsz += 1
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
  
  // - returns a tombed singleton iff 
  //   there is only a single singleton below, tombed or live
  // - returns null iff there are no non-null i-nodes below
  // - otherwise returns a copy of this node such that
  //   all null-i-nodes present when the op began are removed
  // - or this node if there are no null i-nodes below
  //   but more than a single singleton
  final def toTombedCompressed(): BasicNode = {
    val arr = array
    val len = arr.length
    val tmparr = new Array[BasicNode](len)
    var lastsn: SNode[K, V] = null
    var total = 0
    var nulls = 0
    var i = 0
    var bmp = bitmap
    var nbmp = 0
    while (i < len) {
      val sub = arr(i)
      val lsb = bmp & (-bmp)
      bmp ^= lsb
      nbmp |= lsb
      if (sub ne null) {
        tmparr(total) = sub
        total += 1
        sub match {
          case sn: SNode[K, V] => lastsn = sn
          case in: INode[K, V] =>
            val m = /*READ*/in.mainnode
            m match {
              case sn: SNode[K, V] => lastsn = sn
              case _ => // do nothing
            }
        }
      } else nulls += 1
      i += 1
    }
    
    if (total == 0) null
    else if (total == 1 && lastsn != null) lastsn.copyTombed
    else if (nulls > 0) {
      val narr = new Array[BasicNode](total)
      Array.copy(tmparr, 0, narr, 0, total)
      new CNode(nbmp, narr)
    } else this
  }
  
  // - returns a tombed singleton iff 
  //   there is only a single singleton below, tombed or live
  // - returns null iff there are no non-null i-nodes below
  // - returns this node if there is more than a single branch
  //   (even if there are null-i-nodes below - they will not be removed)
  // - otherwise returns a copy of this node such that
  //   all null-i-nodes present when the op began are removed
  final def toWeakTombedCompressed(): BasicNode = {
    val arr = array
    val len = arr.length
    var lastsub: BasicNode = null
    var lastsn: SNode[K, V] = null
    var nulls = 0
    var i = 0
    var bmp = bitmap
    var nbmp = 0
    while (i < len) {
      val sub = arr(i)
      val lsb = bmp & (-bmp)
      bmp ^= lsb
      nbmp |= lsb
      if (sub ne null) {
        if (lastsub ne null) return this
        lastsub = sub
        sub match {
          case sn: SNode[K, V] => lastsn = sn
          case in: INode[K, V] =>
            val m = /*READ*/in.mainnode
            m match {
              case sn: SNode[K, V] => lastsn = sn
              case _ => // do nothing
            }
        }
      } else nulls += 1
      i += 1
    }
    
    if (lastsub eq null) null
    else if (lastsn ne null) lastsn.copyTombed
    else if (nulls > 0) {
      val narr = new Array[BasicNode](1)
      narr(0) = lastsub
      new CNode(nbmp, narr)
    } else this
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
  
  def dual[K, V](x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int, gen: Int): BasicNode = if (lev < 35) {
    val xidx = (xhc >>> lev) & 0x1f
    val yidx = (yhc >>> lev) & 0x1f
    val bmp = (1 << xidx) | (1 << yidx)
    if (xidx == yidx) {
      val subinode = new INode[K, V](gen)//(ConcurrentTrie.inodeupdater)
      subinode.mainnode = dual(x, xhc, y, yhc, lev + 5, gen)
      new CNode(bmp, Array(subinode))
    } else {
      if (xidx < yidx) new CNode(bmp, Array(x, y))
      else new CNode(bmp, Array(y, x))
    }
  } else {
    // sys.error("list nodes not supported yet, lev=%d; %s, %s".format(lev, x.string(lev), y.string(lev)))
    new LNode(x.k, x.v, y.k, y.v)
  }
}


class ConcurrentTrie[K, V] extends ConcurrentTrieBase[K, V] with ConcurrentMap[K, V] {
  root = new INode[K, V](0)
  private val rootupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[ConcurrentTrieBase[_, _]], classOf[INode[_, _]], "root")
  
  @inline private def computeHash(k: K): Int = {
    k.hashCode
  }
  
  /* internal methods */
  
  @tailrec private def inserthc(k: K, hc: Int, v: V) {
    val r = /*READ*/root
    
    // 0) check if the root is a null reference - if so, allocate a new root
    if (r.isNullInode) {
      val ncn = CNode.singular(k, v, hc, 0)
      if (!r.CAS(null, ncn)) inserthc(k, hc, v)
    } else if (!r.fast_insert(k, v, hc, 0, null, r.gen, this)) inserthc(k, hc, v)
  }
  
  @tailrec private def insertifhc(k: K, hc: Int, v: V, cond: AnyRef): Option[V] = {
    val r = /*READ*/root
    
    // 0) check if the root is a null reference - if so, allocate a new root
    if (r.isNullInode) cond match {
      case null | INode.KEY_ABSENT =>
        val ncn = CNode.singular(k, v, hc, 0)
        if (r.CAS(null, ncn)) None
        else insertifhc(k, hc, v, cond)
      case INode.KEY_PRESENT => None
      case otherv: V => None
    } else {
      val ret = r.fast_insertif(k, v, hc, cond, 0, null, r.gen, this)
      if (ret eq null) insertifhc(k, hc, v, cond)
      else ret
    }
  }
  
  /*
  @tailrec private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = /*READ*/root
    if (r eq null) null
    else {
      val res = r.fast_lookup(k, hc, 0, null)
      if (res ne RESTART) res
      else {
        if (r.isNullInode) rootupdater.compareAndSet(this, r, null)
        lookuphc(k, hc)
      }
    }
  }
  */
  
  //@tailrec
  private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = /*READ*/root
    try {
      r.fast_lookup(k, hc, 0, null, r.gen, this)
    } catch {
      case RestartException =>
        if (r.isNullInode) null
        else lookuphc(k, hc)
    }
  }
  
  @tailrec private def removehc(k: K, v: V, hc: Int): Option[V] = {
    val r = /*READ*/root
    val res = r.fast_remove(k, v, hc, 0, null, r.gen, this)
    if (res ne null) res
    else {
      if (r.isNullInode) None
      else removehc(k, v, hc)
    }
  }
  
  private[ctries2] def string = if (root != null) root.string(0) else "<null>"
  
  /* public methods */
  
  final def lookup(k: K): V = {
    val hc = computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }
  
  final override def apply(k: K): V = {
    val hc = computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq null) throw new NoSuchElementException
    else res.asInstanceOf[V]
  }
  
  final def get(k: K): Option[V] = {
    val hc = computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }
  
  override def put(key: K, value: V): Option[V] = {
    val hc = computeHash(key)
    insertifhc(key, hc, value, null)
  }
  
  final override def update(k: K, v: V) {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }
  
  final def +=(kv: (K, V)) = {
    update(kv._1, kv._2)
    this
  }
  
  final override def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k, null.asInstanceOf[V], hc)
  }
  
  final def -=(k: K) = {
    remove(k)
    this
  }
  
  def putIfAbsent(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_ABSENT)
  }
  
  def remove(k: K, v: V): Boolean = {
    val hc = computeHash(k)
    removehc(k, v, hc).nonEmpty
  }
  
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = {
    val hc = computeHash(k)
    insertifhc(k, hc, newvalue, oldvalue.asInstanceOf[AnyRef]).nonEmpty
  }
  
  def replace(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_PRESENT)
  }
  
  def iterator: Iterator[(K, V)] = null // TODO
  
}


object ConcurrentTrie {
  val inodeupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase], classOf[AnyRef], "mainnode")
}


object RestartException extends util.control.ControlThrowable














