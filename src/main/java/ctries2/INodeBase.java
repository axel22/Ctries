package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class INodeBase<K, V> extends BasicNode {
    
    public static final AtomicReferenceFieldUpdater<INodeBase, MainNode> updater = AtomicReferenceFieldUpdater.newUpdater(INodeBase.class, MainNode.class, "mainnode");
    
    public static final Object RESTART = new Object();
    
    public volatile MainNode<K, V> mainnode = null;
    
    public Gen gen;
    
    public INodeBase(Gen generation) {
	gen = generation;
    }
    
    public BasicNode prev() {
	return null;
    }
    
}