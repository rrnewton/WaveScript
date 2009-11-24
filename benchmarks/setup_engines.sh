#!/bin/bash


# Need to tweak this to work for x86 64 too.


if [ `uname -m` == "x86_64" ]; then

  echo DOWNLOADING ENGINE FILES FOR X86_64

#  if ! [ -f libws-SMSegList.1495.O2.traindf.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.traindf.x86_64.a; fi
#  if ! [ -f libws-SMSegList.1495.O2.df.a ];      then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.df.a     ; fi
#  if ! [ -f libws-SMSegList.1495.O2.default.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.default.a; fi

  if ! [ -f libws-SMSegList.newest.O2.coredf.x86_64.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.newest.O2.coredf.x86_64.a; fi
  rm -f libws-SMSegList.newest.O2.coredf.a
  ln -s libws-SMSegList.newest.O2.coredf.x86_64.a libws-SMSegList.newest.O2.coredf.a

  if ! [ -f libws-SMSegList.newest.O2.coredf.nothreads.a ]; 
  then wget http://regiment.us/enginebinaries/libws-SMSegList.newest.O2.coredf.nothreads.x86_64.a; fi
  rm -f libws-SMSegList.newest.O2.coredf.nothreads.a
  ln -s libws-SMSegList.newest.O2.coredf.nothreads.x86_64.a libws-SMSegList.newest.O2.coredf.nothreads.a

  if ! [ -f include_1495.tgz ]; then wget http://regiment.us/enginebinaries/include_1495.tgz; fi
  if ! [ -f include_newest.tgz ]; then wget http://regiment.us/enginebinaries/include_newest.tgz; fi

else

  if ! [ -f libws-SMSegList.1495.O2.traindf.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.traindf.a; fi
  if ! [ -f libws-SMSegList.1495.O2.df.a ];      then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.df.a     ; fi
  if ! [ -f libws-SMSegList.1495.O2.default.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.1495.O2.default.a; fi

  if ! [ -f libws-SMSegList.newest.O2.coredf.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.newest.O2.coredf.a; fi

  if ! [ -f libws-SMSegList.newest.O2.coredf.nothreads.a ]; then wget http://regiment.us/enginebinaries/libws-SMSegList.newest.O2.coredf.nothreads.a; fi

  if ! [ -f include_1495.tgz ]; then wget http://regiment.us/enginebinaries/include_1495.tgz; fi
  if ! [ -f include_newest.tgz ]; then wget http://regiment.us/enginebinaries/include_newest.tgz; fi

fi





if ! [ -d engine ]; then mkdir engine; fi

if ! [ -d ./engine/1495 ]; then
  mkdir engine/1495;
  tar xzvf include_1495.tgz 
  mv include_1495 engine/1495/include
  # Scripts switch this out.
  #ln -s ../../libws-SMSegList.1495.O2.coredf.a engine/newest/libws-SMSegList.a
fi

if ! [ -d ./engine/newest ]; then
  rm -rf engine/newest;
  mkdir engine/newest
  tar xzvf include_newest.tgz 
  mv include engine/newest/include
  ln -s ../../libws-SMSegList.newest.O2.coredf.a engine/newest/libws-SMSegList.a
fi


if ! [ -d ./engine/newest_nothreads ]; then
  rm -rf engine/newest_nothreads;
  mkdir engine/newest_nothreads;
  tar xzvf include_newest.tgz;
  mv include engine/newest_nothreads/include;
  ln -s ../../libws-SMSegList.newest.O2.coredf.nothreads.a engine/newest_nothreads/libws-SMSegList.a
fi


