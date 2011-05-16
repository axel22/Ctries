package ctries2;



import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;



public abstract class INodeBase implements BasicNode {
    
    public static final AtomicReferenceFieldUpdater<INodeBase, Object> updater = AtomicReferenceFieldUpdater.newUpdater(INodeBase.class, Object.class, "mainnode");
    
    public static final Object RESTART = new Object();
    
    public volatile Object mainnode = null;
    
}