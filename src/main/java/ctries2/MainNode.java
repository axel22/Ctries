package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class MainNode<K, V> extends BasicNode {
    
    public static final AtomicReferenceFieldUpdater<MainNode, MainNode> updater = AtomicReferenceFieldUpdater.newUpdater(MainNode.class, MainNode.class, "prev");
    
    public volatile MainNode<K, V> prev = null;
    
    public boolean CAS_PREV(MainNode<K, V> oldval, MainNode<K, V> nval) {
	return updater.compareAndSet(this, oldval, nval);
    }
    
}