package optimization;

import reuters.*;
import java.util.*;

// recording the remaining resouce
public class ResourceMap {
	double totalcost = 0;
	double totallatency = 0;
	
	List<NWVertexResource> vertexresource = null;   // vertex resource usage
	List<NWLinkResource> linkresource = null;		// link resource usage	
 
	ResourceMap(OPNode<Operator> opn, NWVertex nwn) {		
		totalcost = CostFunction.calccost(opn.resource().cpu());
		
		vertexresource = new ArrayList<NWVertexResource>();
		for(int i=0; i<ParseInput.DIM; i++)
			vertexresource.add(null);
				
		if (opn.resource().cpu() > nwn.resource().cpu()) 
			System.err.println("Error: cpu capacity can not support leaf node");		
		vertexresource.set(nwn.id(), new NWVertexResource(opn.resource().cpu()));
		
		if(!opn.isleaf()) {
			linkresource = new ArrayList<NWLinkResource>();
			for(int i=0; i<ParseInput.LINKNUM; i++)
				linkresource.add(null);
		}
	}
	
	// check for constraint: latency, cpu, link
	public boolean constraint(ResourceMap rm, NWPath nwp, Operator op) {
		if(nwp != null) {  // child and parent not on the same node
			// latency constraint
			double newlatency = rm.totallatency + nwp.latency();
			if(!latencyconstraint(newlatency)) return false;
		}
		
		// cpu constraint
		for(int i=0; i<ParseInput.DIM; i++) {			
			if(!cpuconstraint(i, vertexresource.get(i), rm.vertexresource.get(i)))
				return false;
		}
		
		// link constraint
		// two linkresource combination
		if(rm.linkresource != null) {			
			for(int i=0; i<ParseInput.LINKNUM; i++) {
				if(!linkconstraint(i, linkresource.get(i), rm.linkresource.get(i)))
					return false;
			}
		}
		
		if(nwp != null) { // child and parent not on the same node
			// combine with the new link edge
			for(NWLink link : nwp.path()) {
				if(!linkconstraint(op, link.resource(), linkresource, rm.linkresource))
					return false;
			}
		}
		
		//pass all constraint checking		
		return true;
	}
	
	
	
	// check for latency constraint
	private static boolean latencyconstraint(double latency) {
		if (latency > ParseInput.LATENCY) return false;
		else return true;
	}
	
	private static boolean cpuconstraint(int nwnid, NWVertexResource r1, NWVertexResource r2) {
		if ((r1 == null) || (r2 == null)) return true;		
		NWVertex nwn = ParseInput.VITable.get(Integer.valueOf(nwnid));		
		if (nwn.resource().cpu() >= r1.cpu() + r2.cpu()) return true;
		else return false;
	}
	
	private static boolean linkconstraint(int lid, NWLinkResource r1, NWLinkResource r2) {
		if ((r1 == null) || (r2 == null)) return true;
		NWLinkResource lr = ParseInput.LITable.get(Integer.valueOf(lid));
		if (lr.bw() >= r1.bw() + r2.bw() ) return true;
		else return false;
	}
	
	// only one of l1 and l2 can be null
	private static boolean linkconstraint(Operator op, NWLinkResource lr, 
			List<NWLinkResource> l1, List<NWLinkResource> l2) {
		double bw = 0;
		if(l1 == null) {			
			if(l2.get(lr.id()) != null) bw += l2.get(lr.id()).bw();			
		}else if(l2 == null) {			
			if(l1.get(lr.id()) != null) bw += l1.get(lr.id()).bw();			
		}else {
			// both l1 and l2 are not null			
			if(l1.get(lr.id()) != null) bw += l1.get(lr.id()).bw();
			if(l2.get(lr.id()) != null) bw += l2.get(lr.id()).bw();		
		}
		if(lr.bw() >= bw + op.bw()) return true;
		return false;
	}
	
	public void update(ResourceMap rm, NWPath nwp, Operator op, double subcost) {		
		totalcost += subcost;	// total cost
		
		// new latency
		double newlatency;
		if(nwp != null) { newlatency = rm.totallatency + nwp.latency();	}
		else { newlatency = rm.totallatency; }
		totallatency = totallatency < newlatency ? newlatency : totallatency; 
		
		// vertex resource
		for(int i=0; i<ParseInput.DIM; i++) {
			if(vertexresource.get(i) != null)
				vertexresource.get(i).add(rm.vertexresource.get(i));
			else {
				if(rm.vertexresource.get(i) != null) {
					vertexresource.set(i, 
							new NWVertexResource(rm.vertexresource.get(i)));
				}				
			}
		}
		
		// link resource
		if(rm.linkresource != null) {
			for(int i=0; i<ParseInput.LINKNUM; i++) {
				if(linkresource.get(i) != null) 
					linkresource.get(i).add(rm.linkresource.get(i));
				else {
					if(rm.linkresource.get(i) != null)
						linkresource.set(i,
								new NWLinkResource(rm.linkresource.get(i)));
				}
			}
		}
		
		// path link update
		if(nwp != null) { // child and parent not on the same node
			for(NWLink link : nwp.path()) {	
				if (linkresource.get(link.resource().id()) == null)
					linkresource.set(link.resource().id(), new NWLinkResource(op.bw()));
				else
					linkresource.get(link.resource().id()).addbw(op.bw());
			}	
		}
	}
	
	/*
	public boolean cpuconstraint(NWVertex nwn, Operator op) {
		double totalcpu = op.cpu();
		if(vertexresource.get(nwn.id()) != null)
			totalcpu += vertexresource.get(nwn.id()).cpu();
			
		if (nwn.resource().cpu() >= totalcpu) return true;
		else return false;
	}
	
	
	
	// for each path
	public boolean addbw(NWPath nwp, Operator op) {
		for (NWLink nwl : nwp.path()) {
			if(!checkbwl(nwl, op)) return false;
		}
		
		// passing the bw constraint
		for (NWLink nwl : nwp.path()) {
			addbwl(nwl, op);			
		}
		return true;
	}
	
	// nwl: linkresource that add bw to; op: child operator
	private boolean checkbwl(NWLink nwl, Operator op) {
		// id is issued to link resource rather than link
		NWLinkResource lrs = linkresource.get(nwl.resource().id());
		double totalbw;
		if(lrs != null) totalbw = lrs.bw() + op.bw();
		else totalbw = op.bw();
		
		// check bw constraint
		return bwconstraint(nwl.resource(), totalbw);
	}
	
	// nwl: linkresource that add bw to; op: child operator
	private void addbwl(NWLink nwl, Operator op) {
		// id is issued to link resource rather than link
		NWLinkResource lrs = linkresource.get(nwl.resource().id());
		double totalbw;
		if(lrs != null) totalbw = lrs.bw() + op.bw();
		else totalbw = op.bw();
		
		if(lrs != null) lrs.setbw(totalbw);
		else linkresource.set(nwl.resource().id(), 
				new NWLinkResource(totalbw));				
	}
	
	public static boolean bwconstraint(NWLinkResource nwlr, double bw) {
		if(nwlr.bw() >= bw)  return true;
		else return false;
	}
	public boolean addtotallatency() { return false;}
	public boolean addtotalcost() { return false; }*/
}
