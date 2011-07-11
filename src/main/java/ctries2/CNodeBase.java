package ctries2;





public abstract class CNodeBase<K, V> extends BasicNode {
    
    public int bitmap = 0;
    
    public int cachedsize = -1;
    
    public BasicNode[] array = null;
    
}