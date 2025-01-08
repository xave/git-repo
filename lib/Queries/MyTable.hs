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
    -> MyUUID UUID
    -- ^ MyTable UUID
    -> Insert [UUID]
insertMyTable result myUUID =
    Insert
        { iTable = myTableTable
        , iRows =
            [ MyTable
                ()
                (toFields result)
                (MyUUID (sqlUUID (toUUID myUUID)))
                Nothing
                Nothing
            ]
        , iReturning = rReturningI (\(MyTable _ _ (MyUUID mtuuid) _ _) -> mtuuid)
        , iOnConflict = Nothing
        }

-- Select
selectMyTable
    :: UUID
    -- ^ CandidateId
    -> Select MyTableTableR
selectMyTable myUUID = do
    rows@(MyTable _ _ (MyUUID mtuuid) _ _) <- selectTable myTableTable
    where_ (mtuuid .== toFields myUUID)
    pure rows
