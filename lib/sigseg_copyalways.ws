
// Implementing sigsegs in the language:

type Sigseg t = (Array t * Int64);
type Timebase = Int;

namespace Sigseg {

  // A simple copy-always implementation:
  make_nullseg :: () -> Sigseg t;
  nullseg  :: Sigseg t;
  //nullseg  :: () -> Sigseg t;
  timebase :: Sigseg t -> Timebase;
  width    :: Sigseg t -> Int;
  start    :: Sigseg t -> Int64;
  end      :: Sigseg t -> Int64;
  seg_get  :: (Sigseg t, Int) -> t;
  //toSigseg :: (Array t, Int64, Timebase) -> Sigseg t;
  toSigseg :: (Array t, Int64, Timebase) -> Sigseg t;
  toArray  :: Sigseg t -> Array t;

  joinsegs :: (Sigseg t, Sigseg t) -> Sigseg t;
  subseg   :: (Sigseg t, Int64, Int) -> Sigseg t;

  nulltimebase :: Timebase;

  nulltimebase = (0::Int);
  nullseg = (Array:null, (0::Int64))
  fun make_nullseg() (Array:null, (0::Int64))

  fun timebase(_)  nulltimebase;
  fun width((a,_)) Array:length(a);
  fun start((_,st)) st;
  fun end((a,st)) st + intToInt64(Array:length(a) - 1);
  fun seg_get((a,_), ind)    a[ind]
  fun toSigseg(arr, st, tb) (arr, st);
  fun toArray((arr,_)) arr;
  fun subseg((ar,st), pos, len) {
    ind = int64ToInt(pos - st);
    sub = Array:sub(ar, ind, len);
    (sub,pos)
  }
  fun joinsegs((ar1,st1), (ar2,st2)) {
    assert_eq("joinsegs", st2, st1 + intToInt64(Array:length(ar1)));
    (Array:append(ar1,ar2), st1)
  }

  fun Secret:newTimebase(n) n

} // End namespace
