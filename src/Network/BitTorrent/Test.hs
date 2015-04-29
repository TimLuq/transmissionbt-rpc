

First define an auxiliary class Print':
class Print' flag a where
    print' :: flag -> a -> IO ()
 
instance (ShowPred a flag, Print' flag a) => Print a where
    print = print' (undefined::flag)
The main class Print has only one instance, and there is no longer any overlapping. The new class ShowPred has no methods, but its instances precisely mirror those of Show:
-- Just two distinct types
-- alternatively use 'True and 'False with -XDataKinds
data HTrue    
data HFalse   
 
class ShowPred a flag | a->flag where {}
 
                                  -- Used only if the other
                                  -- instances don't apply
-- instance TypeCast flag HFalse => ShowPred a flag -- before -XTypeFamilies
instance (flag ~ HFalse) => ShowPred a flag
 
instance ShowPred Int  HTrue   -- These instances must be
instance ShowPred Bool HTrue   -- the same as Show's
instance ShowPred a flag => ShowPred [a] flag
-- ...etc...
These instances do make use of overlapping instances, but they do not rely on the *context* to distinguish which one to pick, just the instance *head*. Notice that (ShowPred ty flag) always succeeds! If ty is a type for which there is a Show instance, flag gets unified to HTrue; otherwise flag gets unified to HFalse. Now we can write the (non-overlapping) instances for Print':
 instance (Show a) => Print' HTrue a where
   print' _ x = putStrLn (show x)
 instance Print' HFalse a where
   print' _ x = putStrLn "No show method"
