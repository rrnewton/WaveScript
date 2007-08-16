

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


include "timebase.ws";
