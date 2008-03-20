#include "Msp430Adc12.h"

// Module to read our custom audio board for telos.

/* TelosB photo sensors are A4 and A5, audio board is A0 */

module WSMspAdcP {
  provides interface AdcConfigure<const msp430adc12_channel_config_t*>;
}
implementation {

  const msp430adc12_channel_config_t config = {
      inch: INPUT_CHANNEL_A0,
      sref: REFERENCE_AVcc_AVss,
      ref2_5v: REFVOLT_LEVEL_NONE,
      adc12ssel: SHT_SOURCE_SMCLK,
      adc12div: SHT_CLOCK_DIV_1,
      sht: SAMPLE_HOLD_4_CYCLES,
      sampcon_ssel: SAMPCON_SOURCE_SMCLK,
      sampcon_id: SAMPCON_CLOCK_DIV_1
  };
  
  async command const msp430adc12_channel_config_t* AdcConfigure.getConfiguration()
  {
    return &config;
  }
}
