{-# LANGUAGE GHC2021 #-}
-- DerivingStrategies needed to derive Show for newtype
{-# LANGUAGE DerivingStrategies #-}
-- FlexibleInstances needed for makeAdaptorAndInstance
{-# LANGUAGE FlexibleInstances #-}
-- GeneralizedNewtypeDeriving needed to derive Show for newtype
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- MultiParamTypeClasses needed for makeAdaptorAndInstance
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- TemplateHaskell needed for makeAdaptorAndInstance
{-# LANGUAGE TemplateHaskell #-}
-- TypeOperators needed for makeAdaptorAndInstanceInferrable
{-# LANGUAGE TypeOperators #-}
-- UndecidableInstances needed for makeAdaptorAndInstanceInferrable
{-# LANGUAGE UndecidableInstances #-}

module Entities.MyTable
    ( MyTable
    , MyTableT (..)
    , MyTableTableR
    , MyTableTableW
    , myTableTable
    , MyTableId (..)
    , Result (..)
    , MyUUID (..)
    , toUUID
    , toResult
    ) where

import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Time
import Data.UUID
import GHC.Int (Int64)
import Opaleye
import Opaleye.Experimental.Enum
-- NB Opaleye should export Inferrable from a public module
import Opaleye.Internal.Inferrable

-- CREATE TABLE my_table (
--   id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY
--   , result text NOT NULL REFERENCES result_enum (result)
--   , my_uuid uuid REFERENCES other_table (other_table_id)
--   , created_at timestamptz NOT NULL DEFAULT now()
--   , updated_at timestamptz NOT NULL DEFAULT 'epoch'
-- );

newtype MyTableId a = MyTableId a deriving (Show)

$(makeAdaptorAndInstanceInferrable "pMyTableId" ''MyTableId)

newtype MyUUID a = MyUUID a

$(makeAdaptorAndInstanceInferrable "pMyUUID" ''MyUUID)

-- NOTE: This is how I am handling UUID conversion for now.
-- Will be refactored at some point.
class ToUUID a where
    toUUID :: a -> UUID

instance ToUUID (MyUUID UUID) where
    toUUID (MyUUID a) = a

data SqlResult

data Result
    = APPL -- Apple
    | ORNG -- Orange
    | PEAR -- Pear
    deriving (Show)

toResult :: String -> Maybe Result
toResult a = case a of
    "APPL" -> Just APPL
    "ORNG" -> Just ORNG
    "PEAR" -> Just PEAR
    _ -> Nothing

fromResult :: Result -> String
fromResult a = case a of
    APPL -> "APPL"
    ORNG -> "ORNG"
    PEAR -> "PEAR"

sqlResultMapper :: EnumMapper SqlResult Result
sqlResultMapper =
  enumMapper "text" toResult fromResult

instance DefaultFromField SqlResult Result where
  defaultFromField = enumFromField sqlResultMapper

instance result ~ Result
  => D.Default (Inferrable FromField) SqlResult result where
  def = Inferrable D.def

instance D.Default ToFields Result (Field SqlResult) where
  def = enumToFields sqlResultMapper

data MyTableT a b c d e = MyTable
    { mtId :: a
    , mtResult :: b
    , mtMyUUID :: c
    , mtCreatedAt :: d
    , mtUpdatedAt :: e
    }
    deriving (Show)

type MyTable =
    MyTableT
        (MyTableId Int64) -- id
        Result -- result
        (MyUUID UUID) -- my_uuid
        ZonedTime -- created_at
        ZonedTime -- updated_at

type MyTableTableR =
    MyTableT
        (MyTableId (Field SqlInt8))
        (Field SqlResult)
        (MyUUID (Field SqlUuid))
        (Field SqlTimestamptz)
        (Field SqlTimestamptz)

type MyTableTableW =
    MyTableT
        () -- readOnly
        (Field SqlResult)
        (MyUUID (Field SqlUuid))
        (Maybe (Field SqlTimestamptz))
        (Maybe (Field SqlTimestamptz))

$(makeAdaptorAndInstanceInferrable "pMyTable" ''MyTableT)

myTableTable :: Table MyTableTableW MyTableTableR
myTableTable =
    table
        "my_table"
        ( pMyTable
            MyTable
                { mtId = fmap MyTableId (omitOnWriteTableField "id")
                , mtResult = requiredTableField "result"
                , mtMyUUID = pMyUUID (MyUUID (requiredTableField "my_uuid"))
                , mtCreatedAt = optionalTableField "created_at"
                , mtUpdatedAt = optionalTableField "updated_at"
                }
        )
