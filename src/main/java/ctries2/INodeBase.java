package ctries2;





public abstract class INodeBase implements BasicNode {
    
    public static final Object ROOTNULL = new Object();
    
    public static final Object RESTART = new Object();
    
    public volatile Object mainnode = null;
    
}