// $Id: FakeLocation.nc,v 1.1 2004/08/06 16:23:15 newton Exp $

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
 * Provides a "fake" location value to motes based on reading three
 * ADC values for X, Y, and Z coordinates. The TinyViz LocationPlugin
 * sets these values to represent the location of the mote in the display.
 * @author Matt Welsh <mdw@eecs.harvard.edu>
 */
includes FakeLocation;

configuration FakeLocation {
  provides interface Location;
} implementation {
  components Main, FakeLocationM, ADCC;

  Location = FakeLocationM;

  Main.StdControl -> FakeLocationM;
  FakeLocationM.ADC_X -> ADCC.ADC[FAKE_LOCATION_X_PORT];
  FakeLocationM.ADC_Y -> ADCC.ADC[FAKE_LOCATION_Y_PORT];
  FakeLocationM.ADC_Z -> ADCC.ADC[FAKE_LOCATION_Z_PORT];

}
