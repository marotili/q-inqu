module Game.Wire.Extensions 
	( fullFor
	) where

import Control.Wire

-- | Run the passed wire for the full time
fullFor :: (Monad m, HasTime t s) 
	=> t 
	-> Wire s e m a b 
	-> Wire s e m a b
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
			(mx, saveFor x' w')

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

