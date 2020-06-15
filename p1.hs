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

-- Problema 1

dic_imp :: [(String,[String])] -> Dict
dic_imp = Term "".map(bmap id singl).untar.discollect


type Dict = Exp String String

discollect :: (Ord b, Ord a) => [(b,[a])] -> [(b,a)]
discollect = g .! id where
    g (a,x) = [(a,b) | b <- x]


d:: [(String,[String])]
d = [("ABA",["BRIM"]),
     ("ABALO",["SHOCK"]),
     ("AMIGO",["FRIEND"]),
     ("AMOR",["LOVE"]),
     ("MEDO",["FEAR"]),
     ("MUDO",["DUMB","MUTE"]),
     ("PE",["FOOT"]),
     ("PEDRA",["STONE"]),
     ("POBRE",["POOR"]),
     ("PODRE",["ROTTEN"])]


{-dic_norm = collect.filter  p.discollect where
	p(a,b) = a > ""∧ b > ""	 -}


dic_exp :: Dict -> [(String,[String])]
dic_exp = collect . tar


tar = cataExp g where
   g = undefined


dic_rd = undefined



dic_in = undefined
