// $Id: Location.h,v 1.1 2004/08/06 16:23:15 newton Exp $

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
 * This is a generic location datatype. Location dimensions range 
 * from 0.0 to 1.0. 
 * @author Matt Welsh <mdw@eecs.harvard.edu>
 */
typedef struct {
  float x;
  float y;
  float z;
} location_3d_t;

