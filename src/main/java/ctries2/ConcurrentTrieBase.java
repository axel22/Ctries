package ctries2;





// TODO no longer need this
public abstract class ConcurrentTrieBase<K, V> {
    
    public volatile Object root = null;
    
    //protected Object RESTART = INodeBase.RESTART;
    
}