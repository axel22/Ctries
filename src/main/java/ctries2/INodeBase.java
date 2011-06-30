package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class INodeBase extends BasicNode {
    
    public static final AtomicReferenceFieldUpdater<INodeBase, BasicNode> updater = AtomicReferenceFieldUpdater.newUpdater(INodeBase.class, BasicNode.class, "mainnode");
    
    public static final Object RESTART = new Object();
    
    public volatile BasicNode mainnode = null;
    
    public Gen gen;
    
    public INodeBase(Gen generation) {
	gen = generation;
    }
    
    public BasicNode prev() {
	return null;
    }
    
}