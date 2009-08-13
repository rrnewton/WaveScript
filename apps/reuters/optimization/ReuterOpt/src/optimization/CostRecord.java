package optimization;

import reuters.*;

import java.util.*;

public class CostRecord {
	public static HashMap<OPNode<Operator>, List<CostRecord>> SUBTREECOST = 
		new HashMap<OPNode<Operator>, List<CostRecord>>();
	
	public OPNode<Operator> opn;	// the operator node
	public NWVertex nwn;			// network node
	public ResourceMap resource;	// the whole subtree cost
	
	public List<CostRecord> children;	

	
	public CostRecord(OPNode<Operator> opn, NWVertex nwn) {		
		this.opn = opn; this.nwn = nwn; resource = new ResourceMap(opn, nwn);
		if(opn.isleaf()) { children = null; }
		else { children = new ArrayList<CostRecord>(); }
	}
	
	/* simplest case, choose the optimal child position according to 
	 * its subtree cost + cost to the current chosen node
	 * if the constraints are not satisfied, return false 
	 */
	public boolean optcostestimation() {
		
		if (opn.isleaf()) return true;			// if opn is a leaf, no children to place
		
		for(OPNode<Operator> child : opn.children()) {
			boolean childflag = false;			// not found any suitable child position yet
			double subcost = Double.MAX_VALUE;
			CostRecord tempcr = null;
			NWPath temppath = null;
			
			// each child has already haven an entry in SUBTREECOST
			for(CostRecord cr : SUBTREECOST.get(child)) { 				
				NWElement ele = ParseInput.networkpath.get(cr.nwn.id(), nwn.id());
				if(ele == null) continue; // not connected  
				
				// else connected, constraint check
				/*
				 * !!!~~~~ for now, just consider one path, 
				 * If having multiple connecting paths, adding here ~~~~~~!!!!!
				 */
				NWPath nwp = ele.getPath(0);
				if(!resource.constraint(cr.resource, nwp, child.resource()))
					continue;
				
				if(!childflag) { childflag = true; }
				// calculate the total cost
				double tcost = CostFunction.calccost(cr, nwp);
				if (subcost > tcost) { subcost = tcost; tempcr = cr; temppath = nwp; }			
			}		
			
			if(!childflag) // do not find any suitable children position, do not do retreat
				return false;
			
			// update current resource map
			resource.update(tempcr.resource, temppath, tempcr.opn.resource(), subcost);
			children.add(tempcr);
		}
		return true;
	}
}
