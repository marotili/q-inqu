import Control.Monad
import Sound.ALUT

playSound :: IO ()
playSound =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    buffer1 <- createBuffer $ Sine 440 0 1
    buffer2 <- createBuffer HelloWorld
    [source] <- genObjectNames 1
    queueBuffers source [buffer1,buffer2]
    play [source]
    sleep 4
    closeDevice device
    return ()

main = playSound