import Cp
import List  hiding (fac)
import Nat
import BTree
import LTree
import Probability
import ListUtils
import Show
import Data.List hiding (find)
import Test.QuickCheck hiding ((><),choose,collect)
import qualified Test.QuickCheck as QuickCheck
import System.Random  hiding (split)
import System.Process
import GHC.IO.Exception
import Graphics.Gloss
import Control.Monad
import Control.Applicative hiding ((<|>))
import Exp




tipsBdt :: Bdt a -> [a]
tipsBdt = cataBdt (either singl ((uncurry (++)) . p2))
tipsLTree = tips



extLTree :: Bdt a -> LTree a
extLTree = cataBdt g where
  g = either Leaf (Fork . p2)



inBdt = either Dec Bdt


outBdt (Dec a) = Left a
outBdt (Query a (t1,t2))  = Right(a,(t1,t2))


baseBdt f g = id -|- (f >< (g >< g))


recBdt f = baseBdt id f 



cataBdt a = a . (recBdt (cataBdt a)) . outBdt



anaBdt f = inBdt . ( recBdt (anaBdt f)) . f

navLTree :: LTree a -> ([Bool] -> LTree a)
navLTree = cataLTree g 
  where g = either (/x -> const Leaf) curry aux


aux ((l,v),[])= Fork(l [], v [])
aux ((l,v), (h:t)) | h == True  = l t
                   | otherwise = v t 