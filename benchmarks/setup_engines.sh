#!/bin/sh


if ! [ -f libws-SMSegList.1495.O2.traindf.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.traindf.a; fi
if ! [ -f libws-SMSegList.1495.O2.df.a ];      then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.df.a     ; fi
if ! [ -f libws-SMSegList.1495.O2.default.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.default.a; fi

if ! [ -f libws-SMSegList.newest.O2.coredf.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.newest.O2.coredf.a; fi


if ! [ -f include_1495.tgz ]; then wget http://regiment.us/enginebinaries/include_1495.tgz; fi
if ! [ -f include_newest.tgz ]; then wget http://regiment.us/enginebinaries/include_newest.tgz; fi


if ! [ -d engine ]; then mkdir engine; fi

if ! [ -d ./engine/1495 ]; then
  mkdir engine/1495;
  tar xzvf include_1495.tgz 
  mv include_1495 engine/1495/include
fi

if ! [ -d ./engine/newest ]; then
  rm -rf engine/newest;
  mkdir engine/newest
  tar xzvf include_newest.tgz 
  mv include_17?? engine/newest/include
fi
