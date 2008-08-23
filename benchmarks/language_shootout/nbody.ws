/* nbody.sml
 *
 *   N-Body simulation (orbits of Jovian planets).
 *   (Based on SML/NJ version.)
 *
 * Author: Ryan Newton (ryan.newton@alum.mit.edu)



Desired:
-0.169075164
-0.169087605

ocaml:
-0.169075164
-0.169087605

C: 
-0.169075164
-0.169087605

SML output: 
-0.169075164
-0.169031665

scheme
-0.169075163 82852447
-0.169087605 23460595

Hmm, non recursive:
-0.169073079865132

busted:
-0.169289903377906
-0.169289903377906

fixing... still broken
-0.169075164
-0.169051237


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

fun advance(dt) { // one step
  fun pl(i) {
    if i>=N then ()
    else { x[i] += dt*vx[i];
           y[i] += dt*vy[i];
	   z[i] += dt*vz[i];
	   pl (i+1) }
  };
  fun vl(i,j) {
    println("vl "++i++" "++j);
    if i>=N then pl(0)
    else if j>=N then vl(i+1,i+2)
    else {
      let (dx,dy,dz) = (x[i]-x[j], y[i]-y[j], z[i]-z[j]);
      dist = sqrtD(dx*dx + dy*dy + dz*dz);
      mag = dt/(dist*dist*dist);
      let (mi, mj) = (m[i]*mag, m[j]*mag);
      vx[i] -= dx*mj;  vx[j] += dx*mi;
      vy[i] -= dy*mj;  vy[j] += dy*mi;
      vz[i] -= dz*mj;  vz[j] += dz*mi;
      vl (i, j+1)
    }
  };
  vl(0,1)
}

/* calculate initial velocity for the sun */
fun offmoment() {
  fun per(v) (0-v) / SOLAR_MASS;
  fun loop(i, px, py, pz) 
    if i>=N then { 
      vx[0] := per(px); 
      vy[0] := per(py); 
      vz[0] := per(pz) }
    else loop (i+1, px+vx[i]*m[i], py+vy[i]*m[i], pz+vz[i]*m[i]);
  loop (1, 0.0l, 0.0l, 0.0l)
}


fun energy() {
   fun l (i, j, e) 
     if j >= N then l0 (i+1, e)
     else {
       let (dx,dy,dz) = (x[i]-x[j], y[i]-y[j],z[i]-z[j]);
       dist = sqrtD(dx*dx + dy*dy + dz*dz);
       l (i, j+1, e - m[i]*m[j]/dist);
     };
   fun l0(i, e) 
     if i>=N then e
     else {
       let (x, y, z) = (vx[i], vy[i], vz[i]);
       l (i, i+1, e + 0.5l*m[i] * (x*x + y*y + z*z));
     };
  l0(0, 0.0l);
}


fun addloop(i) {
  fun loop(i)   
    if i > 0 then { 
      advance(0.01L);
      loop(i-1)
    };
  loop(i)
}

//printf :: (String, Double) -> () = foreign("printf", ["stdio.h"]);
  
fun run(n) {
  offmoment (); 
  println(energy());
  //printf("%.9f\n", energy());
  //addloop(n);
  advance(0.1L);
  println(energy());
  //printf("%.9f\n", energy());
}

main = {
  run(1000);
  /*
  iterate _ in timer(3) {
    run(50);
  }
  */
  timer$3
}


