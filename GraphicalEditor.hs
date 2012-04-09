import Array
import Control.Monad.Writer  
import Data.Function (on)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

main = interact (unlines . map display . snd . runWriter . foldCmds . takeWhile ( /= Exit) . mapMaybe parse . lines) 

type Color = Char
data Cmd = New Int Int 
         | Clear
         | Exit
         | ColorPix Int Int Color
         | VSeg Int Int Int Color
         | HSeg Int Int Int Color
         | Rect Int Int Int Int Color
         | Fill Int Int Color
         | Save String
         deriving (Show, Eq)

parse :: String -> Maybe Cmd
parse = p . words
p :: [String] -> Maybe Cmd
p ["I",m,n]             = Just $ New (read m) (read n)
p ["C"]                 = Just Clear
p ["L",x,y,[c]]         = Just $ ColorPix (read x) (read y) c
p ["V",x,y1,y2,[c]]     = Just $ VSeg (read x) (read y1) (read y2) c
p ["H",x1,x2,y,[c]]     = Just $ HSeg (read x1) (read x2) (read y) c
p ["K",x1,x2,y1,y2,[c]] = Just $ Rect (read x1) (read x2) (read y1) (read y2) c
p ["F",x,y,[c]]         = Just $ Fill (read x) (read y) c
p ["S",name]            = Just $ Save name
p ["X"]                 = Just $ Exit
p other                 = Nothing

type MyAr = Array (Int, Int) Char

data Image = Image MyAr deriving (Show)
type NamedImage = (String, Image)


foldCmds :: [Cmd] -> Writer [NamedImage] Image
foldCmds cs = foldM applyM initImage cs
-- TODO: don't cheat, making an image that wasn't requested.  
-- The fold's acc should carry a Maybe Image, to represent 
-- the possibility that it hasn't been initialised yet.
initImage = Image $ mkArray (1,1)

white = '0'
applyM :: Image -> Cmd -> Writer [NamedImage] Image
applyM img       (Save n)             = tell [(n,img)] >> return img
applyM img       cmd                  = return $ apply img cmd

apply :: Image -> Cmd -> Image
apply (Image ar) Clear                = Image $ mkArray $ snd $ bounds ar
apply img        (New w h)            = Image $ mkArray (w, h)
apply (Image ar) (ColorPix x y c)     = Image $ ar // [((x,y),c)]
apply (Image ar) (VSeg x y1 y2 c)     = Image $ ar // [((x,y),c) | y<- [y1..y2]]
apply (Image ar) (HSeg x1 x2 y c)     = Image $ ar // [((x,y),c) | x<- [x1..x2]]
apply (Image ar) (Rect x1 x2 y1 y2 c) = Image $ ar // [((x,y),c) | x<- [x1..x2], y<- [y1..y2]]
apply img        Exit                 = error "Program Bug: Cmd 'Exit' applied unexpectedly!"
-- not implemented
apply img        (Fill x y c)         = img

mkArray (w,h) 
  | w < 1 || h < 1 = error $ "Bad bounds: " ++ show (w,h)
  | otherwise      = array ((1,1),(w,h)) [((x,y),white) | x<- [1..w], y<-[1..h]]

display :: (String, Image) ->String
display (n,i) = unlines [n, displayImage i]

displayImage :: Image -> String
displayImage (Image ar) = 
  unlines . 
  map (map snd) .
  groupBy ((==) `on` y) . 
  sortBy (comparing y) .  -- flip assocs to row, col
  assocs $ ar
  where y = snd . fst

