package ctries;



public class Foo {
    public final String a = "init";
    public Foo() {
	System.out.println("before: " + a);
	try {
	    Foo.class.getField("a").set(this, "new value");
	} catch (Exception e) {
	    System.out.println(e);
	}
	System.out.println(a);
    }
}