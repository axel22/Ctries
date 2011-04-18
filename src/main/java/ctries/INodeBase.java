package ctries;





public abstract class INodeBase {
    
    public static final Object NOTFOUND = new Object();
    
    public static final Object RESTART = new Object();
    
    public volatile Object mainnode = null;
    
}