module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List)
import Data.StrMap
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (Unit, ($), (<), (+), (*), (==), show)

-- fromList ((Tuple "k1" 1) : (Tuple "k2" 2) : Nil)
map1 :: StrMap Int
map1 = fromFoldable [ (Tuple "k1" 1)
                    , (Tuple "k2" 2)
                    ]

-- (1: 2: Nil)
values1 :: List Int
values1 = values map1

-- (Just 1)
lookup1 :: Maybe Int
lookup1 = lookup "k1" map1

-- Nothing
lookup2 :: Maybe Int
lookup2 = lookup "k3" map1

-- true
member1 :: Boolean
member1 = member "k1" map1

-- false
member2 :: Boolean
member2 = member "k3" map1

-- 3
fold1 :: Int
fold1 = fold (\a _ v -> a + v) 0 map1

-- 2
fold2 :: Int
fold2 = fold (\a _ v -> a * v) 1 map1

-- [1,2]
foldMap1 :: Array Int
foldMap1 = foldMap (\_ v -> [v]) map1

-- ["k1","k2"]
foldMap2 :: Array String
foldMap2 = foldMap (\k _ -> [k]) map1

-- Nothing を返すとすぐ打ち切る感じ
-- 1
foldMaybe1 :: Int
foldMaybe1 =
  foldMaybe (\a _ v -> if v == 2 then Nothing else Just (a + v)) 0 map1

-- 3
foldMaybe2 :: Int
foldMaybe2 =
  foldMaybe (\a _ v -> if v == 3 then Nothing else Just (a + v)) 0 map1

-- Array.prototype.every 的な
-- true
all1 :: Boolean
all1 = all (\_ v -> v < 3) map1

-- false
all2 :: Boolean
all2 = all (\_ v -> v < 2) map1

-- true
isEmpty1 :: Boolean
isEmpty1 = isEmpty empty

-- false
isEmpty2 :: Boolean
isEmpty2 = isEmpty map1

-- false
isSubmap1 :: Boolean
isSubmap1 = isSubmap map1 (insert "k1" 1 empty)

-- true
isSubmap2 :: Boolean
isSubmap2 = isSubmap (insert "k1" 1 empty) map1

-- 0.0
size1 :: Number
size1 = size empty

-- 1.0
size2 :: Number
size2 = size $ insert "k1" 1 empty

-- 2.0
size3 :: Number
size3 = size map1

-- 3.0
size4 :: Number
size4 = size $ insert "k3" 3 map1

-- "fromList ((Tuple \"k1\" 1) : Nil)"
singleton1 :: String
singleton1 = show $ singleton "k1" 1

-- "fromList ((Tuple \"k1\" 1) : (Tuple \"k2\" 2) : Nil)"
insert1 :: String
insert1 = show $ insert "k2" 2 $ singleton "k1" 1

-- "fromList Nil"
delete1 :: String
delete1 = show $ delete "k1" $ singleton "k1" 1

-- "(Just (Tuple 1 fromList Nil))"
pop1 :: String
pop1 = show $ pop "k1" $ singleton "k1" 1

-- "(Just (Tuple 1 fromList ((Tuple \"k2\" 2) : Nil)))"
pop2 :: String
pop2 = show $ pop "k1" $ map1

-- "Nothing"
pop3 :: String
pop3 = show $ pop "k2" $ singleton "k1" 1

-- "fromList ((Tuple \"k1\" 1) : (Tuple \"k2\" 3) : Nil)"
alter1 :: String
alter1 = show $ alter (\_ -> Just 3) "k2" $ map1

-- "fromList ((Tuple \"k1\" 1) : (Tuple \"k2\" 4) : Nil)"
update1 :: String
update1 = show $ update (\_ -> Just 4) "k2" $ map1

-- "fromList ((Tuple \"k1\" 1) : Nil)"
update2 :: String
update2 = show $ update (\_ -> Nothing) "k2" $ map1

-- "((Tuple \"k1\" 1) : Nil)"
toList1 :: String
toList1 = show $ toList $ update (\_ -> Nothing) "k2" $ insert "k2" 2 $ singleton "k1" 1

-- ["k1","k2"]
keys1 :: Array String
keys1 = keys $ insert "k2" 2 $ singleton "k1" 1

-- "fromList ((Tuple \"k1\" 1) : (Tuple \"k2\" 2) : (Tuple \"k3\" 3) : Nil)"
union1 :: String
union1 = show $ union map1 $ insert "k3" 3 map1

-- "fromList ((Tuple \"k1\" 2) : (Tuple \"k2\" 4) : Nil)"
mapWithKey1 :: String
mapWithKey1 = show $ mapWithKey (\_ v -> 2 * v) map1

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = log $ show map1
