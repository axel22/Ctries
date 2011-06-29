package ctries2;





public interface BasicNode {
    
    public String string(int lev);
    
    public BasicNode prev();
    
    public boolean casPrev(oldval: BasicNode, nval: BasicNode);
    
}