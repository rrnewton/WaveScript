

// A native WS reader for ASCII reuters log files.
// This prototype is hardcoded to only deal with TAQ tuples.

include "stdlib.ws"
include "unix.ws"

DEBUG  = false
DEBUG2 = false

// This is a subset of the fields in Reuters TAQ ticks that we care about:
type TAQ_Tup = 
 (| 
    // They provide three decimal places of accuracy on the second, we simply count by milliseconds:
    TIMESTAMP     : Int64,
    SYMBOL        : String, 
    EXCHTIMESTAMP : Int64,
    RECEIVEDTIME  : Int64, 
    BID           :  Double,
    BIDSIZE       :  Double,
    ASK           :  Double,
    ASKSIZE       : Double
 );
/* Runners up that I thought of including but did not:
 vhSeqNo  BIGINT,
 RecordKey  BIGINT, 
 RTL_Wrap  VARCHAR(2),
 RTL  BIGINT,
 Sub_RTL  VARCHAR(13),
 RuleSetVersion  VARCHAR(13),
 RuleID  VARCHAR(13),
 RuleVersionID  VARCHAR(13),
 RuleClauseNo  VARCHAR(13),
*/


// Parses times of the form 20090630000001.015, that is YYYYDDMMHHMMSS.XXX
//parse_timestamp :: String -> Int64;
parse_timestamp :: Array Char -> Int64;
fun parse_timestamp(arr) {
  //print("parse_timestamp: FIXME: implement this, should parse: "++ String:fromArray(arr) ++ "\n");
  // TODO: what is the proper epoch here?  Should we use the 1970 start?  What does receivedtime use?

  tmp = arr[0];
  fun next(last, next) {
    arr[last]:=tmp; tmp:=arr[next]; arr[next]:='|'; // HACK  
    Int64! Unix:Array:ws_strtoll(arr, last, 10);
  };

  y = next(0,4);
  m = next(4,6);
  d = next(6,8);
  h = next(8,10);
  n = next(10,12);
  s = next(12,14);

  frac = Int64! Unix:Array:ws_strtoll(arr, 15, 10);
  //  print("Fraction "++ Array:sub(arr, 15, 3) ++ "\n");

  // Output in milliseconds:
  (y * 365 * 24 * 60 * 60 * 1000) + // FIXME -- INNACCURATE!!!
  (m *  30 * 24 * 60 * 60 * 1000) + // FIXME -- INNACCURATE!!!
  (d       * 24 * 60 * 60 * 1000) +
  (h            * 60 * 60 * 1000) +
  (n                 * 60 * 1000) +
  (s                      * 1000) + frac
}



// This routine is hardcoded to the schema of TAQ tuples:
// WARNING: this is very low level code that is specific to the wsc2 backend.
// 
read_TAQ_ASCII_tuples :: String -> Stream TAQ_Tup; 
fun read_TAQ_ASCII_tuples(file) {
  using Array;
  using Unix;
  bufsize = 32 * 1024; // 32K constant setting... feel free to change.
  //bufsize = 360; // 32K constant setting... feel free to change.
  iterate x in timer(-1.0) {
    state {
      buf = #[];
      hndl = ptrMakeNull();
      num_bytes = 0; // How many bytes have been filled into the buffer.
      i = 0; // Cursor position within buffer.
      not_init = true;
    }

    //--------------------------------------------------------------------------------
    // Helper function definitions

    fun fill_buffer() {
      if DEBUG then print("Filling buffer from file " ++ hndl  ++"\n");
      if ptrIsNull(hndl) then wserror("File was not opened successfully: " ++ file);
      num_bytes := fread_arr(buf, 1, bufsize, hndl);
      if num_bytes == 0 then { print("End of stream reached.\n"); exit(0); }; // File is finished, should emit EOS.
      i := 0; 
      if DEBUG then print("Got bytes " ++ num_bytes ++"\n");
      if DEBUG then print(" prefix of what we got |"++ String:fromArray(buf.sub(0, min(100, bufsize))) ++"|\n");
    };
    fun is_sep(c) c == ',' || c.charToInt == 10 || c.charToInt == 13;
    // Read a comma separated field, advance the cursor to the next field.
    fun scan_forward() { 
                         while i < num_bytes && not( is_sep( buf[i] )) { i += 1; };
                         //if buf[i].charToInt == 13 then {wserror("DARN carriage return")}; 
                       };
    fun scan_to_eol() { 
      fun scan() { while i < num_bytes && buf[i].charToInt != 10 { i += 1; } };
      old_i = i;  
      scan();
      if i == num_bytes then {
        fill_buffer();
        scan();
        if i == num_bytes then wserror("internal error: single tuple should not be longer than bufsize");
      };
      i += 1;
      if DEBUG then print(" --> SCANNED TO EOL num_bytes="++num_bytes++" old_i = "++old_i++" i = "++i++" buf[i]= "++buf[i].charToInt++" prefix |"++ 
                          String:fromArray(buf.sub(i, min(60, bufsize-i))) ++"|\n");
    };

    fun read_field(allow_empty, reader, default) {
      old_i = i; // At this point the cursor is at the first char of a field.
      // Scout ahead for the next separator character.
      scan_forward();

      // If we hit the end of the buffer before the next separator then we have to abort.
      // We use a trick here.  Fseek backwards a little bit and refill the buffer from the start.
      if i == num_bytes then {
        if DEBUG then print("REWINDING, fseek back "++ old_i - i ++ " chars" ++"\n");
        //code = fseek(hndl, old_i - i, SEEK_SET());
        cur = ftell(hndl);
        code = fseek(hndl, cur - (i - old_i), SEEK_SET());
        if code != 0 then wserror("fseek failed with code: "++code);
        fill_buffer();
        old_i := 0; // Starting over.
        // ASSUMPTION: the read size is much bigger than the individual field, after refilling we WILL have an entire field:
        // We do not need to loop here, if refilling once does not do it there's no hope:
        scan_forward();
        if i == num_bytes then wserror("Error READING got tuple field larger than read buffer: "++ bufsize);
      };
      len = i - old_i;
      // ASSUMES that \r is followed by \n:
      if buf[i].charToInt == 13 then i += 1; // An extra BUMP
      i += 1; // Bump from the separator character to the next field start.
      

      if DEBUG2 then print("\nREADING " ++ (old_i,len) ++" |"++ String:fromArray(buf.sub(old_i,len)) ++"| "); 
      if DEBUG2 then print("   "++ Array:map(charToInt, buf.sub(old_i,len)) ++" EOR\n"); 

      if len == 0 && not(allow_empty) then wserror("empty field not expected, default was "++default++"  BUFFER:\n"++String:fromArray(buf));

      if len == 0 then default 
      else reader(old_i, len);
    };

    //------------------------------------------------------------
    // Extractors for different field types:
    fun skip(i,len) {};
    fun read_int(i,len)    Array:atoi_woffset(buf, i);
    fun read_double(i,len) Array:atof_woffset(buf, i);

    fun read_int64(i,len)  {
      // HACK: FIXME:  I do not know what's with the V prefix on some numbers, just ignoring it for now:
      Array:atoll_woffset(buf, if buf[i] == 'V' then i+1 else i);
    };

    fun read_string(i,len) String:fromArray( buf.sub(i,len) ); // Unnecessary copy!
    fun read_arrstring(i,len) ( buf.sub(i,len) ); // Unnecessary copy!

    // TODO: FIXME: DO SOMETHING TO CATCH MISSING FIELDS THAT WE EXPECT TO BE THERE!!!

    //--------------------------------------------------------------------------------    

    // Initialization:
    if (not_init) then {
      buf := makeUNSAFE(bufsize);
      not_init := false;
      hndl := fopen(file, "r");
      if DEBUG then print("Filling buffer initially\n");
      fill_buffer();
    };

    if DEBUG then print("\nBegin field extraction...\n");
    // Now go through and extract the fields one by one:    
    ts = parse_timestamp$ read_field(false, read_arrstring, #[]); // timeStamp
    for _ = 1 to 2 { read_field(true, skip, ()) }; // eyeCatcher, recType

    sym = read_field(false, read_string, ""); // symbol  VARCHAR(64),
    for _ = 1 to 3 { read_field(true, skip, ()) }; // defName, srcName, vhSeqNo

    exchstamp = read_field(false, read_int64, 0); // exchTimeStamp  BIGINT,
    for _ = 1 to 2 { read_field(true, skip, ()) }; // subType, RecordKey

    rcvdtm = read_field(false, read_int64, 0); // ReceivedTime  BIGINT

    // COLLECT_DATETIME_2, RTL_Wrap RTL Sub_RTL, RuleSetVersion, RuleID, RuleVersionID, RuleClauseNo, 
    // RecordType, RecordStatus, EditType, SOURCE_DATETIME, SEQNUM, TRDXID_1    
    for _ = 1 to 14 { read_field(true, skip, ()) }; 

    // FIXME: TEMP: ALLOWING THESE TO BE EMPTY (zero) FOR NOW:
    bid     = read_field(true, read_double, 0); // BID  DOUBLE,    
    bidsize = read_field(true, read_double, 0); // BIDSIZE  DOUBLE,

    for _ = 1 to 2 { read_field(true, skip, ()) }; // BID_MMID1, NO_BIDMMKR

    // FIXME: TEMP: ALLOWING THESE TO BE EMPTY (zero) FOR NOW:
    ask     = read_field(true, read_double, 0); // ASK  DOUBLE,    
    asksize = read_field(true, read_double, 0); // ASKSIZE  DOUBLE,
/*
 ASK_MMID1  VARCHAR(32),
 NO_ASKMMKR  BIGINT,
 MID_PRICE  DOUBLE,
 ....
*/
    scan_to_eol();

    // TEMP: FIXME: For now we do a simple filter for tuples that have a bid and ask
    if bid != 0 && ask != 0 then 
    emit (| TIMESTAMP     = ts
          , SYMBOL        = sym
          , EXCHTIMESTAMP = exchstamp
          , RECEIVEDTIME  = rcvdtm
          , BID           = bid
          , BIDSIZE       = bidsize
          , ASK           = ask
          , ASKSIZE       = asksize
          )
    else if DEBUG then print(" ==> TUPLE FILTERED, NOT EMITTED\n");
  }
}



main1 = iterate _ in FINITE_BURST(15) {
  println("Testing ATOI: " ++ Unix:String:atoi_woffset("23101blah", 2));
  emit 99;
}

//main = read_TAQ_ASCII_tuples("small_prefix.log")
//main0 = read_TAQ_ASCII_tuples("TAQ_only.log")
//main0 = read_TAQ_ASCII_tuples("test.txt")
main0 = read_TAQ_ASCII_tuples("only_TAQ_373mb.log")

//main1 = snoop_every(100, (fun (i,_) show(i)), main0)

main1 = iterate _ in main0 { 
          state{ i = 0 } 
          i += 1; 
          if i == 100 then { 
            i := 0;
            emit ();
          }
        }

// Count how many different symbols
main2 = iterate tup in main0 {
  state { i = 0;
          c = 0;
          tbl = HashTable:make(1000) }
  using HashTable;
  tbl.set(tup.SYMBOL, 1);
  i += 1;
  if i == 100 then {
    i := 0;
    c += 1;
    acc = 0;
    foreach(fun(key,_) { 
        acc += 1;
      }, tbl);
    //print(acc ++ " entries in table\n");
    emit acc;
  }
}

//main = main0
