module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Loops
import Data.IORef
import Data.Maybe (fromJust)
import Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
    GLFW.init
    window <- fmap fromJust $ createWindow 600 600 "GLFW" Nothing Nothing
    makeContextCurrent (Just window)

    oldTime <- newIORef (0 :: Double)

    whileM_ (not <$> windowShouldClose window) $ do

        fromTime <- readIORef oldTime
        time <- fmap fromJust getTime
        let dt = time - fromTime
            fps = round $ 1 / (dt / 2)
        writeIORef oldTime time

        -- render stuff here

        swapBuffers window

        pollEvents
        threadDelay 10000
    
    destroyWindow window
    terminate
