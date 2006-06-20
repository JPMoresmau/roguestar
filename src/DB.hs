module DB 
    (DB,
     initial_db, 
     DB_BaseType,
     ObjectRef(..), 
     ObjectThing(..),
     dbNextRandomInteger,
     dbNextRandomIntegerStream,
     dbAddObjectThing)
    where
    
import CreatureData
import Control.Monad.State
import System.Time
import RNG
import Data.Map as Map

data ObjectRef = CreatureRef Integer
		 deriving (Eq,Ord,Read,Show)

data ObjectThing = CreatureThing Creature
		   deriving (Read,Show)

-- |
-- Random access form of the roguestar database.
--
data DB_BaseType = DB_BaseType { random_number_stream_stream :: [[Integer]],
				 next_object_ref :: Integer,
			         dbtable :: Map ObjectRef ObjectThing }

-- |
-- Serial form of the roguestar database.
--
data DB_Persistant_BaseType = DB_Persistant_BaseType { random_number_generator_seed_ :: Integer,
                                                       next_object_ref_ :: Integer,
						       dbtable_ :: [(ObjectRef,ObjectThing)] }
                              deriving (Read,Show)

toPersistant :: DB_BaseType -> DB_Persistant_BaseType
toPersistant db = DB_Persistant_BaseType {
					  random_number_generator_seed_ = (random_number_stream_stream db) !! 0 !! 0,
					  next_object_ref_ = next_object_ref db,
					  dbtable_ = toList (dbtable db)
					 }

fromPersistant :: DB_Persistant_BaseType -> DB_BaseType
fromPersistant persistant = DB_BaseType {
					 random_number_stream_stream = randomIntegerStreamStream (random_number_generator_seed_ persistant),
					 next_object_ref = next_object_ref_ persistant,
					 dbtable = fromList (dbtable_ persistant)
					}

fromPersistant_tupled :: (DB_Persistant_BaseType,String) -> (DB_BaseType,String)
fromPersistant_tupled (persistant,str) = (fromPersistant persistant,str)

instance Read DB_BaseType where
    readsPrec n = \x -> Prelude.map fromPersistant_tupled (readsPrec n x)

instance Show DB_BaseType where
    show db = show (toPersistant db)

type DB a = State DB_BaseType a

-- |
-- Generates an initial DB state.
--
initial_db :: IO DB_BaseType
initial_db = do (TOD seconds picos) <- getClockTime
		return DB_BaseType { random_number_stream_stream = randomIntegerStreamStream (seconds + picos),
				     next_object_ref = 0,
				     dbtable = (fromList [])
				   }



-- |
-- Gets the next ObjectRef integer, after incrementing it.
--
dbNextObjectRef :: DB Integer
dbNextObjectRef = do db <- get
		     let result = next_object_ref db
			 in do put (db { next_object_ref=(succ result) })
			       return result

-- |
-- Adds an object to the database.  The first parameter transforms an Integer into an ObjectRef,
-- the second is an object appropriately encapsulated in an ObjectThing.  The object is added to the
-- database under the resulting ObjectRef.
--
dbAddObjectThing :: (Integer -> ObjectRef) -> ObjectThing -> DB ObjectRef
dbAddObjectThing mkobjref thing = do ref <- dbNextObjectRef
				     db <- get
				     put (db { dbtable=(insert (mkobjref ref) thing (dbtable db))})
				     return (mkobjref ref)


-- |
-- Generates and returns the next random Integer.
--
dbNextRandomInteger :: DB Integer
dbNextRandomInteger = do db <- get
			 let rngss0 = random_number_stream_stream db 
                             (rngs0,rngss1) = (head rngss0, tail rngss0)
                             (result,rngs1) = (head rngs0, tail rngs0)
                             in do put db { random_number_stream_stream=(rngs1:rngss1) }
                                   return (result)

-- |
-- Generates a returns a random integer stream.
--
dbNextRandomIntegerStream :: DB [Integer]
dbNextRandomIntegerStream = do db <- get
                               let rngss = random_number_stream_stream db
                                   in do put db { random_number_stream_stream=(tail rngss) }
                                         return (head rngss)

