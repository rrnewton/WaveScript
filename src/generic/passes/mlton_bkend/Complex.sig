(**
 ** $Id: Complex.sig 89 2005-12-16 17:00:43Z rlpm $
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

signature COMPLEX =
sig
  type complex

  val i : complex

  val fromReal : Real32.real -> complex
  val fromInt : int -> complex
  
  val magnitude : complex -> Real32.real
  val phase : complex -> Real32.real

  val fromPolar : {magnitude:Real32.real,phase:Real32.real} -> complex

  val toString : complex -> string

  val ~ : complex -> complex

  (* complex conjugate *)
  val conj : complex -> complex
  val ~~ : complex -> complex

  val + : complex * complex -> complex
  val - : complex * complex -> complex
  val * : complex * complex -> complex
  val / : complex * complex -> complex

  val exp : complex -> complex
  val ln : complex -> complex

  val pow : complex * complex -> complex

  val sin : complex -> complex
  val cos : complex -> complex
end
