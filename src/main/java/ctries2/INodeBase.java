package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class INodeBase implements BasicNode {
    
    public static final AtomicReferenceFieldUpdater<INodeBase, BasicNode> updater = AtomicReferenceFieldUpdater.newUpdater(INodeBase.class, BasicNode.class, "mainnode");
    
    public static final Object RESTART = new Object();

    public static final BasicNode INVALIDATED = new BasicNode() {
	    public String string(lev: Int) = throw new UnsupportedOperationException();
	    public BasicNode prev() = throw new UnsupportedOperationException();
	    public boolean casPrev(x: BasicNode, y: BasicNode) = throw new UnsupportedOperationException();
	};
    
    public volatile BasicNode mainnode = null;
    
    public int gen;
    
    public INodeBase(int generation) {
	gen = generation;
    }
    
    public BasicNode prev() {
	return null;
    }
    
}