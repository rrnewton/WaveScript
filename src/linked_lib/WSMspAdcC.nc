
// Module to read our custom audio board for telos.

generic configuration WSMspAdcC() {
  provides interface Read<uint16_t>;
  provides interface ReadStream<uint16_t>;

  provides interface Resource;
  provides interface ReadNow<uint16_t>;
}
implementation {
  components new AdcReadClientC();
  Read = AdcReadClientC;

  components new AdcReadStreamClientC();
  ReadStream = AdcReadStreamClientC;

  components WSMspAdcP;
  AdcReadClientC.AdcConfigure -> WSMspAdcP;
  AdcReadStreamClientC.AdcConfigure -> WSMspAdcP;

  components new AdcReadNowClientC();
  Resource = AdcReadNowClientC;
  ReadNow = AdcReadNowClientC;
  
  AdcReadNowClientC.AdcConfigure -> WSMspAdcP;
}

