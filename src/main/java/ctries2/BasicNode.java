package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class BasicNode {
    
    public static final AtomicReferenceFieldUpdater<BasicNode, BasicNode> updater = AtomicReferenceFieldUpdater.newUpdater(BasicNode.class, BasicNode.class, "prev");
    
    public abstract String string(int lev);
    
    public volatile BasicNode prev = null;
    
    public boolean CAS_PREV(BasicNode oldval, BasicNode nval) {
	return updater.compareAndSet(this, oldval, nval);
    }
    
}