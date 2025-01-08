module Queries.MyTable
    ( insertMyTable
    , selectMyTable
    ) where

-- MyTable
import Entities.MyTable

import Data.Text as T
import Data.UUID
import Opaleye

-- Insert
insertMyTable
    :: Result
    -- ^ Result
    -> MyUUID
    -- ^ MyTable UUID
    -> Insert [UUID]
insertMyTable result myUUID =
    Insert
        { iTable = myTableTable
        , iRows =
            [ MyTable
                ()
                (sqlStrictText (T.pack $ show result))
                (sqlUUID (toUUID myUUID))
                Nothing
                Nothing
            ]
        , iReturning = rReturningI (\(MyTable _ _ mtuuid _ _) -> mtuuid)
        , iOnConflict = Nothing
        }

-- Select
selectMyTable
    :: UUID
    -- ^ CandidateId
    -> Select MyTableTableR
selectMyTable myUUID = do
    rows@(MyTable _ _ mtuuid _ _) <- selectTable myTableTable
    where_ (mtuuid .== toFields myUUID)
    pure rows
