module DB 
    (DB,
     initial_db, 
     DB_BaseType,
     ObjectRef(..), 
     ObjectThing(..),
     dbAddObjectThing,
     nextRandomInteger,
     dbNextRandomInteger)
    where
    
import CreatureData
import Control.Monad.State
import System.Time
import Data.Map as Map

data ObjectRef = CreatureRef Integer
		 deriving (Eq,Ord,Read,Show)

data ObjectThing = CreatureThing Creature
		   deriving (Read,Show)

-- |
-- Random access form of the roguestar database.
--
data DB_BaseType = DB_BaseType { random_number_generator_seed :: Integer,
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
					  random_number_generator_seed_ = random_number_generator_seed db,
					  next_object_ref_ = next_object_ref db,
					  dbtable_ = toList (dbtable db)
					 }

fromPersistant :: DB_Persistant_BaseType -> DB_BaseType
fromPersistant persistant = DB_BaseType {
					 random_number_generator_seed = random_number_generator_seed_ persistant,
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
		return DB_BaseType { random_number_generator_seed = seconds + picos,
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
-- Generates the next in a sequence of random Integers.
--
nextRandomInteger :: Integer -> Integer
nextRandomInteger x = (x * 0x5DEECE66D + 0xB) `mod` (2^48)

-- |
-- Generates and returns the next random Integer.
--
dbNextRandomInteger :: DB Integer
dbNextRandomInteger = do db <- get
			 let next_seed = nextRandomInteger $ random_number_generator_seed db
			     in do put db { random_number_generator_seed=next_seed }
				   return next_seed
