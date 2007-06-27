(**
 ** $Id: Complex.sml 89 2005-12-16 17:00:43Z rlpm $
 **
 ** A complex number library for Standard ML
 ** Author: Rory McGuire <rlpm at cs dot unm dot edu>
 **
 ** This program is free software; you can redistribute it and/or
 ** modify it under the terms of the GNU General Public License as
 ** published by the Free Software Foundation; either version 2 of the
 ** License, or (at your option) any later version.
 **
 ** This program is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 ** General Public License for more details.
 **
 ** You can obtain a copy of the GPL at:
 **   http://www.fsf.org/copyleft/gpl.html.
 **)

structure Complex : COMPLEX =
struct
  infix 6 + -
  infix 7 * /
  
  type complex = {real:Real32.real, imag:Real32.real}

  val i = {real= Real32.fromInt 0, imag= Real32.fromInt 1}

  fun fromReal r = {real=r, imag= Real32.fromInt 0}

  val fromInt = fromReal o Real32.fromInt

  fun magnitude {real,imag} =
      Real32.Math.sqrt(Real32.+(Real32.*(real,real),Real32.*(imag,imag)))

  fun phase {real,imag} =
      if Real32.==(imag,0.0)
      then if Real32.signBit real
           then Real32.Math.pi
           else 0.0
      else if Real32.==(real,0.0)
      then if Real32.signBit imag
           then Real32.~(Real32./(Real32.Math.pi,2.0))
           else Real32./(Real32.Math.pi,2.0)
      else
          let
              val a = Real32.Math.atan (Real32./(imag,real))
          in
              if Real32.signBit real
              then if Real32.signBit imag
                   then a - Real32.Math.pi
                   else a + Real32.Math.pi
              else a
          end

  fun toPolar x =
      {magnitude=magnitude x, phase=phase x}

  fun fromPolar {magnitude,phase} =
      {real=Real32.*(magnitude,Real32.Math.cos phase),
       imag=Real32.*(magnitude,Real32.Math.sin phase)}

  fun toString {real,imag} =
      Real32.toString real ^ "+" ^ Real32.toString imag ^ "i"

  fun ~ {real,imag} =
      {real=Real32.~(real),imag=Real32.~(imag)}

  fun conj {real,imag} =
      {real=real,imag=Real32.~(imag)}

  val ~~ = conj

  fun op+ ({real=r1,imag=i1},{real=r2,imag=i2}) =
      {real=Real32.+(r1,r2),imag=Real32.+(i1,i2)}
  
  fun op- ({real=r1,imag=i1},{real=r2,imag=i2}) =
      {real=Real32.-(r1,r2),imag=Real32.-(i1,i2)}

  fun op* ({real=x,imag=y},{real=a,imag=b}) =
      {real=Real32.-(Real32.*(x,a),Real32.*(y,b)),
       imag=Real32.+(Real32.*(y,a),Real32.*(x,b))}
  
  fun op/ ({real=x,imag=y},{real=a,imag=b}) =
      let
          fun f x = Real32./(x,Real32.+(Real32.*(a,a),Real32.*(b,b)))
      in
          {real=f (Real32.+(Real32.*(x,a),Real32.*(y,b))),
           imag=f (Real32.+(Real32.*(y,a),Real32.*(x,b)))}
      end
  

  fun exp {real,imag} =
      fromPolar{magnitude=Real32.Math.exp(real),phase=imag}
 
  fun ln x =
      let
          val {magnitude,phase} = toPolar x
      in
          {real=Real32.Math.ln magnitude,imag=phase}
      end
 
  fun pow (z, {real=x,imag=y}) =
      let
          val {magnitude=a,phase=p} = toPolar z
          val lna = Real32.Math.ln a
      in
          exp({real=Real32.-(Real32.*(x,lna),Real32.*(p,y)),
               imag=Real32.+(Real32.*(p,x),Real32.*(y,lna))})
      end

  fun sin {real=x,imag=y} =
      {real=Real32.*(Real32.Math.sin x,Real32.Math.cosh y),
       imag=Real32.*(Real32.Math.cos x,Real32.Math.sinh y)}

  fun cos {real=x,imag=y} =
      {real=Real32.*(Real32.Math.cos x,Real32.Math.cosh y),
              imag=Real32.~(Real32.*(Real32.Math.sin x,Real32.Math.sinh y))}
end
