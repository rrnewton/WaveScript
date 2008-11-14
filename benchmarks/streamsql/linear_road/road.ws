

/*
	int	m_iType;   // Type:
				   //	. 0: position report
				   //	. 2: account balance request
				   //	. 3: daily expenditure request
				   //	. 4: travel time request			 
	int	m_iTime;   // 0...10799 (second), timestamp position report emitted
	int	m_iVid;	   // 0...MAXINT, vehicle identifier
	int	m_iSpeed;  // 0...100, speed of the vehicle
	int	m_iXway;   // 0...L-1, express way
	int	m_iLane;   // 0...4, lane
	int	m_iDir;    // 0..1, direction
	int	m_iSeg;    // 0...99, segment
	int	m_iPos;    // 0...527999, position of the vehicle
	int	m_iQid;    // query identifier
	int m_iSinit;  // start segment
	int	m_iSend;   // end segment
	int	m_iDow;    // 1..7, day of week
	int	m_iTod;    // 1...1440, minute number in the day
	int	m_iDay;    // 1..69, 1: yesterday, 69: 10 weeks ago
*/


include "matrix.ws"
include "matrix3D.ws"

L = 2;

type Type = Uint8;
type Time = Uint16;
type Vid = Int;
type Speed = Uint8;
type Xway = Uint8;
type Lane = Uint8;
//type Dir = Bool;
type Dir = Uint8;
type Seg = Uint8;
type Pos = Int;
type Qid = Int;
//type Send = Uint8;
type Dow = Uint8;
type Tod = Uint16;
type Day = Uint8;
// It would be very nice to read these straight from disk as a record type:
type InputTuple = (Type * Time * Vid * Speed * Xway * Lane * Dir * Seg * Pos * Qid * Seg * Seg * Dow * Tod * Day);

type Location = (Xway * Seg * Dir);

stopped_thresh = 3;

// Here are the major pieces of state:

type VEntry = ( LASTLOC: Location, 
                LASTSPEED: Speed,
                BALANCE: Int, 
                STOPPED: Int,
		CURTOLL: Int );

vehicle_table :: HashTable (Vid, VEntry) = HashTable:make(100);
stopped_cars  :: HashTable (Vid, Bool)   = HashTable:make(100);

accidents :: HashTable (Location, Bool)  = HashTable:make(100);

// segstats maps a location to an array[5] for the last 5 minutes
type SegStat = ( NUMCARS: Int, 
                 SPEEDSUM: Int );
//segstats :: HashTable (Location, Array SegStat) = HashTable:make(100);
// This is dense, so we index a 3d matrix by Location: Xway/Seg/Dir:
//segstats :: Matrix3D (Array SegStat) = Matrix3D:build(L, 100, 2, fun(i,j,k) Array:make(5, (NUMCARS=0)))
segstats :: Matrix3D (Array SegStat) = Matrix3D:create(L, 100, 2, #[])

// When could one garbage collect for vehicles that haven't been seen in a while?
days_of_tollhist = 70;
//type TollStat = Int
// Map: Vid -> Xway -> Day in the last 70 days -> TollStat
// The representation is a hash table mapping Vid onto an L x 70 matrix.
tollstats :: HashTable (Vid, Matrix Int) = HashTable:make(100);

//================================================================================

//file = "datafile20seconds_spaces.dat"
file = "datafile3hours_spaces.dat"

input :: Stream InputTuple = readFile(file, "mode: text", timer(1000));

fun position_report(first, mega :: InputTuple) {
  using HashTable;
  let (typ, time, vid, speed, xway, lane, dir, seg, pos, qid, startseg, endseg, dayofweek, timeofday, day) = mega;
  curloc = (xway, seg, dir);

  // Initialize:  
  if first then { 
    // This should use a dense representation:
    /*    
    for xway = 1 to L {
      for seg = 0 to 99 {
        for dir = 0 to 1 {
	  segstats.set((xway,seg,dir), Array:make())
	}
      }
    }
    */
    Matrix3D:map_inplace(segstats, fun(_) Array:make(5, (NUMCARS=0, SPEEDSUM=0)));
    println("Done initializing.\n");
  };
    
  if contains(vehicle_table, vid) then {
    old = get(vehicle_table, vid);

    let (toll, stoppedcnt) = 
      if old.LASTLOC == curloc
      then { 

        fun getsegstat(loc) {
	  let (x, s, d) = loc;
	  Matrix3D:get(segstats, Int!x, Int!s, Int!d)
	}; 
        
	// If the vehicle has moved between segments, charge a toll:
	
	// TODO: Charge the toll from the LAST segment, and compute the toll for the current one.
	// TODO: if it's NOT currently in an exit lane:

	//numvehicles = (segstats.get(curloc))[0].NUMCARS;
	//thisseg = Matrix3D:get(segstats, Int!xway, Int!seg, Int!dir);
	thisseg = getsegstat(curloc);
	numvehicles = thisseg[0].NUMCARS + 1;
	
	// Write the new count:
	speedsum = thisseg[0].SPEEDSUM + Int! speed;
	thisseg[0] := (NUMCARS= numvehicles, SPEEDSUM= speedsum);

	//let (oldxway, oldseg, olddir) = old.LASTLOC;
	// Subtract from the old segment:
	//Array:set(Matrix3D:get(segstats, Int!xway, Int!seg, Int!dir));
	oldseg = getsegstat(old.LASTLOC);
	oldseg[0] := ( oldseg[0] | NUMCARS := oldseg[0].NUMCARS - 1,
	                           SPEEDSUM := oldseg[0].SPEEDSUM - Int!old.LASTSPEED);
	//Array:set(getsegstat(old.LASTLOC), 0, (NUMCARS = 99));
	
	// GET AVERAGE SPEED, CHECK IF HIGHER THAN 40.
	avgspeed = speedsum / numvehicles;

	toll = 
	  if avgspeed < 40
	  then 2 * (numvehicles - 50) * (numvehicles - 50)
  	  else 0;

	println(" Toll: "++ toll ++ " numcars " ++numvehicles ++  " avg speed " ++ avgspeed);

        (toll, old.STOPPED + 1)
      }
      else (0,0);

    set(stopped_cars, vid, stoppedcnt >= stopped_thresh);
    set(vehicle_table, vid, ( old | BALANCE := old.BALANCE + 1, 
                                    STOPPED := stoppedcnt,
				    CURTOLL := toll));
  } else 
  // This is the first time we've seen this vehicle!
  {
    tollmatrix = Matrix:create(L, days_of_tollhist, 0);
    set(tollstats, vid, tollmatrix);

    set(vehicle_table, vid, (| LASTLOC= curloc, BALANCE= 99, STOPPED= 0, CURTOLL= 0, LASTSPEED= speed));
  };
}

fun account_balance_request(mega) {
  print("...ACCT BALANCE REQUEST...\n");
}

fun daily_expenditure_request(mega) {
  print("...DAILY EXPENDITURE REQUEST...\n");
}

fun travel_time_request(mega) {
  print("...TRAVEL TIME REQUEST...\n");
}

one_big_op = 
iterate megatup in input {
  state { first = true }

  // println $ megatup;

  let (typ, time, vid, speed, xway, lane, dir, seg, pos, qid, startseg, endseg, dayofweek, timeofday, day) = megatup;
  
  if typ == 0 then position_report(first, megatup) else
  if typ == 1 then account_balance_request(megatup) else
  if typ == 2 then daily_expenditure_request(megatup) else
                   travel_time_request(megatup);

  first := false;

  //emit (typ, time, vid, speed, xway, lane, dir, seg, pos, qid, startseg, endseg, dayofweek, timeofday, day);
  //emit vehicle_table
  //emit tollstats
  emit ()
}

main = one_big_op
 
