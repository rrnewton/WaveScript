package reuters;

import java.io.OutputStream;
import java.util.*;

// to be extended to support multiple roots later
// return a topological walk of the tree

public class OPTree<T> {
	private OPNode<T> root;
	
	public OPTree(OPNode<T> root) { this.root = root; }	
	public OPNode<T> root() { return root; }
	
	// return the OPTree<T> as a list of OPNode<T>, by a post-order
	public List<OPNode<T>> toList() {
		List<OPNode<T>> list = new ArrayList<OPNode<T>>();
		postorder(root, list);
		return list;
	}

	private void postorder(OPNode<T> root, List<OPNode<T>> list) {
		if (root.isleaf()) {list.add(root); return; }
		for(OPNode<T> n : root.children) { postorder(n, list); }
		list.add(root);
	}
	
	public void pirnt(OutputStream ops) {
		List<OPNode<T>> list = toList();	
		for(OPNode<T> n : list) { n.pirnt(ops);	}
	}
}
