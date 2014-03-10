{-# LANGUAGE FlexibleContexts #-}
module Game.Wire.Extensions 
	( fullFor
	) where

import Control.Wire

-- | Run the passed wire for the full time
fullFor :: (Fractional t, Monoid e, Monad m, HasTime t (Timed NominalDiffTime ())) 
	=> t 
	-> Wire (Timed NominalDiffTime ()) e m a b 
	-> Wire (Timed NominalDiffTime ()) e m a b
fullFor x w = mkGen $ \ds a -> do
	let x' = x - realToFrac (dtime ds)
	let ds' = if x' <= 0 
		then Timed (fromRational (toRational x)) ()
		else ds

	(mx, w') <- stepWire w ds' (Right a)

	return $ if x' <= 0 
		then
			(mx, mkEmpty)
		else
			(mx, fullFor x' w')

--fullAfter x w = mkGen $ \ds a -> do
--	let x' = x - realToFrac (dtime ds)
--	let ds' = if x' <= 0 
--		then Timed (fromRational (toRational x)) ()
--		else ds

--	(mx, w') <- stepWire w ds' (Right a)

--	return $ if x' <= 0 
--		then
--			(mx, mkEmpty)
--		else
--			(mx, saveFor x' w')

