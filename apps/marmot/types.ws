

// ID, X,Y, YAW, and AML result.
type NodeRecord = (Int * Float * Float * Float);

type AML       = (Array Float * Int64 * Timebase);
type IntAML    = (Array Int16 * Int64 * Timebase);
type Detection = List (Sigseg Int16);
//  Includes a timestamp
type LikelihoodMap = (Matrix Float * Int64);

type Tagged t = (NodeRecord * t);

type AxesBounds = (Float * Float * Float * Float);
type Settings   = (AxesBounds * (Float * Float));
//type Converter  = Int -> Float; // Coordinate converter.
// Xbound, Ybound, and conversion procs.
//type CoordSystem = (Int * Int * Converter * Converter);
//type CoordSystem = (Int * Int * Float * Float * Float * Float);


LOG_TIMING = 255;
// from elog.h in emstar
LOG_EMERG = 0;       /**< system is unusable */
LOG_ALERT = 1;       /**< action must be taken immediately */
LOG_CRIT = 2;      /**< critical conditions */
LOG_ERR = 3;      /**< error conditions */
LOG_WARNING = 4;       /**< warning conditions */
LOG_NOTICE = 5;      /**< normal but significant condition */
LOG_INFO = 6;      /**< informational */
LOG_DEBUG_0 = 7;       /**< debug */
LOG_OFF =  -1;	/**< don't emit log messages */
LOG_UNDEFINED = -2;      /**< loglevel not specified */


tb_cpu :: Timebase = Secret:newTimebase(1);
tb_gps :: Timebase = Secret:newTimebase(2);
tb_vxp :: Timebase = Secret:newTimebase(3);
tb_globalvxp :: Timebase = Secret:newTimebase(4);

nullterm = String:implode([intToChar(0)])

//include "timebase.ws";
include "config.ws";
