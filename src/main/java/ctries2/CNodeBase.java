package ctries2;





public abstract class CNodeBase<K, V> extends BasicNode {
    
    public int bitmap = 0;
    
    public BasicNode[] array = null;
    
    public volatile BasicNode prev = null;
    
}