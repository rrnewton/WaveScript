// $Id: FakeLocationM.nc,v 1.1 2004/08/06 16:23:15 newton Exp $

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
module FakeLocationM {
  provides {
    interface StdControl;
    interface Location;
  }
  uses interface ADC as ADC_X;
  uses interface ADC as ADC_Y;
  uses interface ADC as ADC_Z;

} implementation {

  enum {
    FAKE_LOCATION_ADC_SCALING = 65535,
    FAKE_LOCATION_SCALE_X = 100,
    FAKE_LOCATION_SCALE_Y = 100,
    FAKE_LOCATION_SCALE_Z = 100
  };

  location_3d_t cur_loc;

  command result_t StdControl.init() {
    cur_loc.x = 0;
    cur_loc.y = 0;
    cur_loc.z = 0;
    return SUCCESS;
  }
  command result_t StdControl.start() {
    return SUCCESS;
  } 
  command result_t StdControl.stop() {
    return SUCCESS;
  } 

  command result_t Location.getLocation() {
    return call ADC_X.getData();
  }

  event result_t ADC_X.dataReady(uint16_t val) {
    cur_loc.x = val / (FAKE_LOCATION_ADC_SCALING*1.0);
    cur_loc.x *= FAKE_LOCATION_SCALE_X;
    if (!call ADC_Y.getData()) {
      signal Location.locationDone(NULL);
    }
    return SUCCESS;
  }
  event result_t ADC_Y.dataReady(uint16_t val) {
    cur_loc.y = val / (FAKE_LOCATION_ADC_SCALING*1.0);
    cur_loc.y *= FAKE_LOCATION_SCALE_Y;
    if (!call ADC_Z.getData()) {
      signal Location.locationDone(NULL);
    }
    return SUCCESS;
  }
  event result_t ADC_Z.dataReady(uint16_t val) {
    cur_loc.z = val / (FAKE_LOCATION_ADC_SCALING*1.0);
    cur_loc.z *= FAKE_LOCATION_SCALE_Z;
    signal Location.locationDone(&cur_loc);
    return SUCCESS;
  }

}
