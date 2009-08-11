




mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 0
  where   
   fn = magnitude
   loop i z count
    | i == max_depth = count
    | fn(z) >= 2.0   = count 
    | otherwise      = loop (i+1) (z*z + c) (count+1)

type Pair = (Word16, Word16)

mandelProg :: Int -> Int -> Int -> GraphCode Int
mandelProg max_row max_col max_depth = 
    do --w <- newWorld
--        position :: TagCol  Pair                  <- newTagCol()
--        dat      :: ItemCol Pair (Complex Double) <- newItemCol()
--        pixel    :: ItemCol Pair Int              <- newItemCol()   

       position  <- newTagCol()
       dat       <- newItemCol()
       pixel     <- newItemCol()   
       
       let mandelStep tag = 
	    do cplx <- get dat tag
	       put pixel tag (mandel max_depth cplx)

       prescribe position mandelStep 

       gcPrintWorld "1 "
       gcPutStr "Initialize 1: "

       initialize $ 
        for_ 0 max_row $ \i -> 
         for_ 0 max_col $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do --seq (unsafePerformIO (putStrLn (show (_i,_j)))) $ put dat (_i,_j) z
	     put dat (_i,_j) z
	     call position (_i,_j)

--        initialize $ 
--         forM [(0,0), (1,1)]
-- 	  (\ (_i,_j) ->
-- 	   do put dat (_i,_j) 39.9
-- 	      call position (_i,_j))

--        initialize $ sequence $
--          [do put dat (0,0) 39.38
-- 	     call position (0,0)]
-- 	  do put dat (1,1) 39.38
-- 	     call position (1,1)]

--        initialize $ 
--          do put dat (0,0) 39.38
-- 	    call position (0,0) 

       gcPrintWorld " 2 "
       gcPutStr  "Now finalize 2: "

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ 
	foldRange 0 max_row (return 0) $ \acc i -> 
	 foldRange 0 max_col acc $ \acc j -> 
	   do cnt <- acc
	      p <- get pixel (fromIntegral i, fromIntegral j)
	      if p == max_depth
   	       then return (cnt + (i*max_col + j))
   	       else return cnt
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


runMandel = do (a:b:c:_) <- System.getArgs 
	       check <- runGraph $ mandelProg (read a) (read b) (read c)
	       putStrLn ("Mandel check " ++ show check)

testMandel = do check <- runGraph $ mandelProg 3 3 3
	        putStrLn ("Mandel check " ++ show check)

t = testMandel
