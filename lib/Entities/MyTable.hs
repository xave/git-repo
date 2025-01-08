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

import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time
import Data.UUID
import GHC.Int (Int64)
import Opaleye

-- CREATE TABLE my_table (
--   id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY
--   , result text NOT NULL REFERENCES result_enum (result)
--   , my_uuid uuid REFERENCES other_table (other_table_id)
--   , created_at timestamptz NOT NULL DEFAULT now()
--   , updated_at timestamptz NOT NULL DEFAULT 'epoch'
-- );

newtype MyTableId = MyTableId Int64 deriving (Show)
data MyUUID = MyUUID UUID

-- NOTE: This is how I am handling UUID conversion for now.
-- Will be refactored at some point.
class ToUUID a where
    toUUID :: a -> UUID

instance ToUUID MyUUID where
    toUUID (MyUUID a) = a

data Result
    = APPL -- Apple
    | ORNG -- Orange
    | PEAR -- Pear
    deriving (Show)

toResult :: Text -> Maybe Result
toResult a = case a of
    "APPL" -> Just APPL
    "ORNG" -> Just ORNG
    "PEAR" -> Just PEAR
    _ -> Nothing

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
        MyTableId -- id
        Result -- result
        MyUUID -- my_uuid
        ZonedTime -- created_at
        ZonedTime -- updated_at

type MyTableTableR =
    MyTableT
        (Field SqlInt8)
        (Field SqlText)
        (Field SqlUuid)
        (Field SqlTimestamptz)
        (Field SqlTimestamptz)

type MyTableTableW =
    MyTableT
        () -- readOnly
        (Field SqlText)
        (Field SqlUuid)
        (Maybe (Field SqlTimestamptz))
        (Maybe (Field SqlTimestamptz))

$(makeAdaptorAndInstanceInferrable "pMyTable" ''MyTableT)

myTableTable :: Table MyTableTableW MyTableTableR
myTableTable =
    table
        "my_table"
        ( pMyTable
            MyTable
                { mtId = omitOnWriteTableField "id"
                , mtResult = requiredTableField "result"
                , mtMyUUID = requiredTableField "my_uuid"
                , mtCreatedAt = optionalTableField "created_at"
                , mtUpdatedAt = optionalTableField "updated_at"
                }
        )
instance Default ToFields MyTableId (Field SqlInt8) where
    def = toToFields $ toFields . getRecId
      where
        getRecId (MyTableId a) = a

instance DefaultFromField SqlInt8 MyTableId where
    defaultFromField = fmap MyTableId fromPGSFromField
