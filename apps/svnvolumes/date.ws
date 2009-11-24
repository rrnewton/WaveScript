

namespace Date {
  //let months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  // Statically summing these to get cumulative numbers:
  monthTable = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]; 

  // Convert 'Y-M-D' date to total number of days.
  // Doesn't take leap years into account!!!
  toDays :: (Int * Int * Int) -> Int;
  fun toDays( (y,m,d) ) {
    365*y + 
    monthTable`List:ref(m-1) +
    d-1
  }


}



/*



  (define months '(31 28 31 30 31 30 31 31 30 31 30 31))  
  (define (total-days y m d) 
    (+ (* 365 y)
       (apply + (list-head months (sub1 m)))
       (sub1 d)))
  (define birth (total-days 1980 06 25))

  (define granularity 1/365)
  
  ;(define (date y m d) (- (total-days y m d) birth))
  (trace-define (date y m d) 
    (/ (- (total-days y m d) birth)
       365.0
       ))

*/

