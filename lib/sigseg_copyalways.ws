
// Implementing sigsegs in the language:

type Sigseg t = (Array t * Int64);
type Timebase = Int;

type SS t = Sigseg t;
type TB = Timebase;

namespace Sigseg {

  // A simple copy-always implementation:
  make_nullseg :: () -> SS t;
  nullseg  :: SS t;
  //nullseg  :: () -> SS t;
  timebase :: SS t -> TB;
  width    :: SS t -> Int;
  start    :: SS t -> Int64;
  end      :: SS t -> Int64;
  seg_get  :: (SS t, Int) -> t;
  //toSigseg :: (Array t, Int64, TB) -> SS t;
  toSigseg :: (Array t, Int64, Timebase) -> SS t;
  toArray  :: SS t -> Array t;

  joinsegs :: (SS t, SS t) -> SS t;
  subseg   :: (SS t, Int64, Int) -> SS t;

  nulltimebase = 0;
  nullseg = (Array:null,nulltimebase)
  fun make_nullseg() (Array:null,nulltimebase)

  fun timebase(_)  0;
  fun width((a,_)) Array:length(a);
  fun start((_,st)) st;
  fun end((a,st)) st + gint(Array:length(a)) - 1;
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

} // End namespace
