


c_array_to_ptolemy :: (Int, String, (Array Int16), Int) -> () = 
  foreign("send_int16_array_to_ptolemy", []);

ptolemy_open :: (String, Int) -> Int =
  foreign("ptolemy_open", []);


fun array_to_ptolemy(handle, name, a) {
   c_array_to_ptolemy(handle, name, a, a`Array:length)
}

