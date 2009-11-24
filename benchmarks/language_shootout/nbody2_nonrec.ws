/* nbody.sml
 *
 *   N-Body simulation (orbits of Jovian planets).
 *   (Based on SML/NJ version.)
 *   This version is converted to use loops rather than recursion.
 *
 * Author: Ryan Newton (ryan.newton@alum.mit.edu)
 */

include "stdlib.ws"

SOLAR_MASS = 4.0L * piD * piD
DAYS_PER_YEAR = 365.24L

// For each planet, x,y,z, vx,vy,vz, and mass
bodies :: List (Double * Double * Double * Double * Double * Double * Double);
bodies =  // sun,  jupiter,  saturn,  neptune,  uranus
        [(0.0l,  0.0l,  0.0l,  0.0l,  0.0l,  0.0l, 1.0l),
	 (4.84143144246472090l, -1.16032004402742839l, -1.03622044471123109e-1l, 
	  1.66007664274403694e-3l, 7.69901118419740425e-3l, -6.90460016972063023e-5l, 
	  9.54791938424326609e-4l), 
	 (8.34336671824457987l, 4.12479856412430479l, -4.03523417114321381e-1l, 
	  -2.76742510726862411e-3l, 4.99852801234917238e-3l, 2.30417297573763929e-5l, 
	  2.85885980666130812e-4l), 
	 (1.28943695621391310e1l, -1.51111514016986312e1l, -2.23307578892655734e-1l, 
	  2.96460137564761618e-3l, 2.37847173959480950e-3l, -2.96589568540237556e-5l, 
	  4.36624404335156298e-5l), 
	 (1.53796971148509165e1l, -2.59193146099879641e1l, 1.79258772950371181e-1l, 
	  2.68067772490389322e-3l, 1.62824170038242295e-3l, -9.51592254519715870e-5l, 
	  5.15138902046611451e-5l)]

// Ugly, but we don't have records yet.
fun _x ((x,y,z,vx,vy,vz,m)) x
fun _y ((x,y,z,vx,vy,vz,m)) y
fun _z ((x,y,z,vx,vy,vz,m)) z
fun _vx((x,y,z,vx,vy,vz,m)) vx
fun _vy((x,y,z,vx,vy,vz,m)) vy
fun _vz((x,y,z,vx,vy,vz,m)) vz
fun _m ((x,y,z,vx,vy,vz,m)) m

fun id(x) x
fun sm (x) x * SOLAR_MASS;
fun dpy(x) x * DAYS_PER_YEAR;
fun selall(f,g) List:toArray$ map(compose(f,g),bodies)

N = List:length $ bodies;
x  = selall(id,  _x)
y  = selall(id,  _y)
z  = selall(id,  _z)
vx = selall(dpy, _vx)
vy = selall(dpy, _vy)
vz = selall(dpy, _vz)
m  = selall(sm,  _m)



/*
fun run(n) {
  offmoment (); 
  //e1 = energy();  
  //printf("%.9f\n", energy());
  println(energy());

  addloop(n);
  println(energy());
  //printf("%.9f\n", energy());
};

*/

printf :: (String, Double) -> () = foreign("printf", ["stdio.h"]);

main = {
  //run(1000);
  iterate _ in timer(3) {

  //    run(1000);

// FIXME BUG: Moving these function defs back and forth between outside and inside the iterate changes the RESULT!
fun advance(dt) { // one step
    i = 0;
    j = 1;
    while i < N {
      //println("vl "++i++" "++j);
      if j >= N then {
	j := i + 2;
        i += 1;
      } else {
        let (dx,dy,dz) = (x[i]-x[j], y[i]-y[j], z[i]-z[j]);
        dist = sqrtD(dx*dx + dy*dy + dz*dz);
        mag = dt/(dist*dist*dist);
        let (mi, mj) = (m[i]*mag, m[j]*mag);
        vx[i] -= dx*mj;  vx[j] += dx*mi;
        vy[i] -= dy*mj;  vy[j] += dy*mi;
        vz[i] -= dz*mj;  vz[j] += dz*mi;
	j += 1;
      }
    };
    for i = 0 to N-1 {
      x[i] += dt*vx[i];
      y[i] += dt*vy[i];
      z[i] += dt*vz[i];
    }
};
//fun addloop(n) { println("n "++n); for i = 1 to n { print("loop "++i++"\n"); advance(0.01L) } };
fun addloop(n) { for i = 1 to n { advance(0.01L) } };
fun offmoment() { /* calculate initial velocity for the sun */
  let (px, py, pz) = (0.0l, 0.0l, 0.0l);
  for i = 1 to N-1 { 
    px += vx[i] * m[i];
    py += vy[i] * m[i];
    pz += vz[i] * m[i];
  };
  fun per(v) (0-v) / SOLAR_MASS;
  vx[0] := per(px); 
  vy[0] := per(py); 
  vz[0] := per(pz) 
};
fun energy() {
  e = 0.0l;
  for i = 0 to N-1 {
   let (_x, _y, _z) = (vx[i], vy[i], vz[i]);
   e += 0.5l * m[i] * (_x * _x + _y * _y + _z * _z);
   for j = i+1 to N-1 {
       let (dx,dy,dz) = (x[i]-x[j], y[i]-y[j], z[i]-z[j]);
       dist = sqrtD(dx*dx + dy*dy + dz*dz);
       e -= m[i]*m[j]/dist;
   }
  }; 
  e
};

  offmoment (); 
  //println(energy());
  printf("%.9f\n", energy());

__NUM = 20 * 1000 * 1000;

  addloop(__NUM);
  //advance(0.1L);
  //println(energy());
  printf("%.9f\n", energy());

    wserror$""
  }
}


/*

scheme advance once 
-0.16907516382852447
-0.16905419139548536

nonrec, C:
-0.169075163828524
-0.169055928089136

nonrec, ws.early:
-0.16907516382852447
-0.1690559280891364

vl 0 1
vl 0 2
vl 0 3
vl 0 4
vl 0 5
vl 1 7
vl 2 9
vl 3 11
vl 4 13

CORRECt:
vl 0 1
vl 0 2
vl 0 3
vl 0 4
vl 0 5
vl 1 2
vl 1 3
vl 1 4
vl 1 5
vl 2 3
vl 2 4
vl 2 5
vl 3 4
vl 3 5
vl 4 5
vl 5 6


*/
