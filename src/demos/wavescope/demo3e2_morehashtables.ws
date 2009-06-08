



main = iterate _ in timer(0) {
  using HashTable;
  ht = make(10);

  {
    print("Initial table: "++ ht ++ "\n");  
    ht.set(5,"flub");
    ht.set(17,"bub");  
    print("Set two: "++ ht ++ "\n");  
    ht.set(17,"blub");  
    print("Set again: "++ ht ++ "\n");  

    ht.set(27,"collide1");  
    ht.set(7,"collide2");  
    print("Added more, w/collision "++ ht ++ "\n");  

    print("do gets "++ (ht.get(5), ht.get(17)) ++"\n");

    for i = 1 to 10 {
      set(ht,i,"");
    };

    //let (sz,dat) = ht;
    //dat[0] := resize(dat[0]);

    //print("resized to "++ dat[0].Array:length ++ "\n  "++ ht ++"\n");
    print("resized to "++ ht ++"\n");

    ht.rem(17);
    print("removed "++ ht  ++"\n");

    // get(ht,99);
  };

  {
    ht = make(10);
    ht.set(#["goo"] ,      "a");
    ht.set(#["goo","boo"], "b");
    ht.set(#[],            "c");
    print("\nNow more complex key type: " ++ ht ++ "\n");

    print("Set in a pure way:\n");
    ht2 = set_pure(ht, #["goo"] , "aaa");
    //ht2 = ht;
    print(ht ++"\n");
    print(ht2 ++"\n");

    print("\n\n Print all contents:\n");
    foreach(fun(k,v) v.print, ht2);
    print("\n\n");

    /*    
    keyyy = #["goo", "boo"];
    myhsh = absI$ Internal:hash(keyyy);    
    print(myhsh);
    print("\n");
    print(myhsh.moduloI(10));
    print("\n");
    //set_BANG(ht, myhsh ,      "a");
    set_BANG(ht, #["goo","boo"] ,      "a");
    print(ht);
    print("\n");
    */
  };
  
  //emit 99;
  emit ();
}

