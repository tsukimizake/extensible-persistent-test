{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( someFunc
    ) where
import Data.Extensible
import Data.Functor.Identity
import Control.Lens hiding ((:>))
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Aeson (FromJSON(..), withObject)
import Data.Proxy
import Data.String
import GHC.TypeLits

type Person = Record '["name" :> String, "age" :> Int]
type WPFields= '["job" :> String, "name" :> String, "age" :> Int]
type WorkerPerson = Record WPFields
type MaybeWorkerPerson = RecordOf Maybe WPFields

--data family AgeFieldToMaybe a
--data instance AgeFieldToMaybe a = a -- 右辺が型パラメタだとダメ

class AgeFieldToMaybe a where
  type Hoge (a :: *)
  type Hoge a = a

instance AgeFieldToMaybe Int where 
  type Hoge Int = Int

--instance AgeFieldToMaybe ("age" :> Int) where 
--  type Hoge ("age" :> Int) = ("age" :> Maybe Int) 



p1 :: Person
p1 = #name @= "hoge" <: #age @= 42 <: nil

p2 :: WorkerPerson
p2 = #job @= "MV" <: #name @= "huga" <: #age @= 10 <: nil

p3 :: WorkerPerson
p3 = #job @= "MV" <: nil `happend` p1

p4 :: WorkerPerson
p4 = p3 & #job .~ "NC"
p6 :: MaybeWorkerPerson
p6 = #job @= Just "MV" <: #name @= Just "huga" <: #age @= Just 10 <: nil

changeWorker :: (Lookup s "job" String) => Record s -> Record s
changeWorker x = x & #job .~ "MV"

type Animal = Record
  [ "name" :> String
  , "collective" :> String
  , "cry" :> Maybe String
  ]

collectiveOf :: Animal -> String
collectiveOf a = unwords ["a", a ^. #collective, "of", a ^. #name ++ "s"]

collectiveOf2 :: (Lookup s "name" String , Lookup s "collective" String) => Record s -> String
collectiveOf2 a = unwords ["a", a ^. #collective, "of", a ^. #name ++ "s"]

someFunc :: IO ()
someFunc = do
  print $ p2
  print $ (shrink p2 :: Person)
  print p3
  print p4
  print $ changeWorker p4
  let p5 = hmap id p4
  print p5
  let a1 = #name @= "dove"
            <: #collective @= "dule"
            <: #cry @= Just "coo"
            <: nil
  print a1
  putStrLn $ collectiveOf2 a1
