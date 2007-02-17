module DriverData (DataFreshness(..)) where

-- |
-- [@Fresh@] guarantees that the data returned will be exactly the data that is in the engine, but this data is less likely to be immediately available; send a driverRequest* is necessary.
-- [@New@] return new data, but the engine might have changed since then; send a driverRequest* is necessary.
-- [@Old@] return data that is at least one turn old, if the data isn't available there is no way to request it from the engine
-- [@Anything@] return new data if available, otherwise old data
--
data DataFreshness = Fresh | New | Old | Anything deriving (Show,Eq)