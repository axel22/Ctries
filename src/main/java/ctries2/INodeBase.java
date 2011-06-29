package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class INodeBase extends BasicNode {
    
    public static final AtomicReferenceFieldUpdater<INodeBase, Object> updater = AtomicReferenceFieldUpdater.newUpdater(INodeBase.class, Object.class, "mainnode");
    
    public static final Object RESTART = new Object();
    
    public volatile Object mainnode = null;
    
    public int gen;
    
    public INodeBase(int generation) {
	gen = generation;
    }
    
}