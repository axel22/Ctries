package ctries2;





public abstract class ConcurrentTrieBase<K, V> {
    
    public volatile INode<K, V> root = null;
    
    protected Object RESTART = INodeBase.RESTART;
    
}