package reuters;

import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

public class OPNode<T> {
	
	private static int index = 0;
	
	int id;
	OPNode<T> parent = null;
	List<OPNode<T>> children = null;
	
	T op;
	
	public OPNode(T op) { this.op = op; id = index++; }	
	public void setparent(OPNode<T> parent) { this.parent = parent; }
	public void addchild(OPNode<T> child) {
		if (children == null) children = new ArrayList<OPNode<T>>();
		children.add(child);
	}
	
	public List<OPNode<T>> children() { return children; }
	
	public boolean isleaf() { return children == null; }
	public boolean isroot() { return parent == null; }
	public int id() { return id; }
	public T resource(){ return op; }
	
	public void pirnt(OutputStream ops) {
		try {
			ops.write(("Node: " + String.valueOf(id) + 
					op.toString() + "\n").getBytes());
		}catch(IOException e) { e.printStackTrace(); }
	}
}
