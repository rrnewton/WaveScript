// $Id: Location.nc,v 1.1 2004/08/06 16:23:15 newton Exp $

/* Copyright (c) 2002 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704. Attention: Intel License Inquiry.  
 * 
 * Author: Matt Welsh <mdw@eecs.harvard.edu>
 */

/**
 * @author Matt Welsh <mdw@eecs.harvard.edu>
 */


includes Location;

/**
 * Generic interface for a node to determine its 3d location in 
 * physical space.
 */
interface Location {

  command result_t getLocation();
  event void locationDone(location_3d_t *loc);

}
