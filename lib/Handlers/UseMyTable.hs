{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.UseMyTable
    ( getMyTables
    ) where

import Entities.MyTable
import Queries.MyTable

import Control.Monad.Reader
import Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import Data.Maybe
import Data.Pool (withResource)
import Data.Pool.Internal
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import Data.Time
import Data.UUID
import Database.PostgreSQL.Simple (ConnectInfo (..))
import qualified Database.PostgreSQL.Simple as PG (Connection, close, connect)
import qualified Network.Wai as Wai
import qualified Opaleye as O
import Opaleye.Internal.Inferrable (Inferrable)

-- MyTable
getMyTables
    :: ( WithDB env m
       , MonadFail m
       , (Default (Inferrable O.FromField) O.SqlTimestamptz ZonedTime)
       )
    => MyUUID UUID
    -> m [MyTable]
getMyTables myUUID = do
    rows <- runSelectWithPool $ selectMyTable (toUUID myUUID)
    mtIds <- pure $ mtId <$> rows
    uuids <- pure $ (MyUUID . mtId) <$> rows
    liftIO $ pure $ rows

-------------------------------------------------------
-- Handler Helper funcs
-------------------------------------------------------
class Has field env where
    obtain :: env -> field

type DBPool = Pool PG.Connection

-------------------------------------------------------
-- NOTE: This won't compile because the app context has not been bootstrapped
-- instance Has DBPool (AppContext m) where obtain = appDBPool

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

-- | Constraint for monadic actions that wants access to database.
type WithDB env m = (MonadReader env m, Has DBPool env, MonadIO m)

runUpdateWithPool :: WithDB env m => O.Update haskells -> m haskells
runUpdateWithPool update = do
    pool <- grab @DBPool
    -- pool <- ask appDBool
    liftIO $ withResource pool $ \conn -> O.runUpdate conn update
{-# INLINE runUpdateWithPool #-}

runInsertWithPool :: WithDB env m => O.Insert haskells -> m haskells
runInsertWithPool insert = do
    pool <- grab @DBPool
    liftIO $ withResource pool $ \conn -> O.runInsert conn insert
{-# INLINE runInsertWithPool #-}

runDeleteWithPool :: WithDB env m => O.Delete haskells -> m haskells
runDeleteWithPool delete = do
    pool <- grab @DBPool
    liftIO $ withResource pool $ \conn -> O.runDelete conn delete
{-# INLINE runDeleteWithPool #-}

runSelectWithPool :: (WithDB env m, Default (Inferrable O.FromFields) fields haskells) => O.Select fields -> m [haskells]
runSelectWithPool select = do
    pool <- grab @DBPool
    liftIO $ withResource pool $ \conn -> O.runSelectI conn select
{-# INLINE runSelectWithPool #-}
