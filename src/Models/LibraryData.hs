module Models.LibraryData
    (LibraryModel(..))
    where
    
data LibraryModel = QuestionMark
                  | AscensionClassStarship       -- spaceships
                  | Encephalon                   -- creatures
                  | Recreant
                  | PhasePistol                  -- guns
                  | Arm LibraryModel             -- bits and pieces of other things, identified this way so we don't have to list each one individually
                  | Shoulder LibraryModel
                  deriving (Eq,Ord,Show)
                  
-- I have drawings of these but haven't modeled them yet
-- There's room for 72 handheld weapons, based on energy type (electricity,kinetic,fire,cold).
-- firing pattern (pulse,beam,wave) and size (pistol,carbine,rifle), and power (moderate,strong).
-- Phase weapons occupy the electricity/beam/moderate power niche. 
-- phaser 
-- phase rifle

-- I also have drawings for the following player races: myrmidon, anachronid, androsynth
-- Ascendants are just light effects so they don't need a model, exactly, but a routine to do the light effect.

-- So I need models for these:
-- perrenial (plant creature)
-- reptilian (obvious)
-- Hellion (these could be anything, but they should be good with their hands and eyes since they have high scores in dex and perception)
-- Goliath (could be anything, but they have high strength and constitution)
-- kraken (something creepy with tenticles, perhaps)
-- Caduceator (snakes in a game!)

-- Of course we also need monsters, grenade and rocket launchers, all kinds of scientific equipment,
-- spaceships, buildings (with square bases, so they fit on a single tile), etc, and we need different
-- versions of these for different factions and alignments.