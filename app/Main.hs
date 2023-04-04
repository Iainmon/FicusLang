module Main where
import Prelude (IO, putStrLn)
-- import Graphics.Blank                     -- import the blank canvas
-- import Shapes
-- import Data.Text (pack)
-- -- putImageDataAt :: (ImageData, Double, Double) -> Canvas ()

-- dot :: Double -> Double -> Canvas ()
-- dot x y = do 
--   fillRect(x,y,1,1)
--   lineWidth 1;
--   stroke();
--   -- beginPath()
--   -- rect (x,y,x + 1,y + 1)
--   -- closePath()
--   -- fillStyle "yellow";
--   -- fill()
--   -- lineWidth 1;
--   -- strokeStyle "black";
--   -- stroke();
--   -- lineWidth 2
--   -- strokeStyle "black"
--   -- stroke()
-- dots [] = return ()
-- dots ((a,b):xs) = do
--   -- fillRect(a,b,a + 1,b + 1)
--   -- stroke()
--   setColor (Color 1.0 0.8 0.2)
--   dot a b
--   dots xs


-- plotMask :: Mask -> Canvas ()
-- plotMask (M f) = do
--   let dom = [(x,y) | x <- [0.0..size], y <- [0.0..size]]
--   dots [(x,y) | (x,y) <- dom, f (x/size,y/size) > 0.5]
--   where size = 300.0

-- setColor :: Color -> Canvas ()
-- setColor c = do 
--   strokeStyle $ pack $ colorToHex c
--   fillStyle $ pack $ colorToHex c

-- testMask = M f
--   where f (x,y) = (cos x' * 0.5 + 0.5) * (sin y' * 0.5 + 0.5)
--           where x' = x * zoom
--                 y' = y * zoom
--                 zoom = 10.0

-- main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
--         send context $ do                 -- send commands to this specific context
--                 -- moveTo(50,50)
--                 -- lineTo(200,100)
--                 -- lineWidth 10
--                 -- strokeStyle "red"
--                 -- stroke()                  -- this draws the ink into the canvas
--                 dot 110 110
--                 dot 200 100
--                 dots [(x,(cos x) + 100) | x <- [0..500]]
--                 setColor (Color 1.0 0.8 0.2)
--                 plotMask testMask
                
--                 -- mapM_ (\(a,b) -> dot a b) [(100 * x,100 * y) | x <- map cos pts, y <- map sin pts]
-- -- module Main where



-- -- import Shapes

main :: IO ()
main = putStrLn "Hello, Haskell!"
