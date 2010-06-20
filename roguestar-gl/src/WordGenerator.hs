module WordGenerator
    (HardOrSoft(..),
     WordGenerator(..),
     generateWord,
     sample_word_generator)
    where

import System.Random
import Data.Either

-- |
-- Works typically are formed from alternating instances
-- of hard (consonents) and soft (vowels) letters.
-- Using HardOrSoft we specify word patterns such as
-- [[Hard,Soft,Hard],[Hard,Soft,Hard,Soft]].
-- This way we can specify how long words typically are
-- and how they are patterned.  (As an example, Hebrew
-- traditionally had rather strict rules on vowel-consonent 
-- patterns.)
-- 
data HardOrSoft = Hard
                | Soft

-- |
-- Specifies the hard letter patterns, soft letter patterns,
-- and word patterns or either HardOrSoft or a specific
-- string to be inserted at a specific place in a word.
--
data WordGenerator = WordGenerator { hards, softs :: [String], word_patterns :: [[Either HardOrSoft String]] }

-- |
-- Generate a word from a NameGenerator and a pseudo-random seed.
--
generateWord :: WordGenerator -> StdGen -> String
generateWord (WordGenerator { word_patterns=[] }) _ = ""
generateWord word_gen _ | maximum (map length $ word_patterns word_gen) == 0 = ""
generateWord word_gen std_gen | minimum (map length $ word_patterns word_gen) == 0 = generateWord 
										     (word_gen { word_patterns=filter ((== 0) . length) $ word_patterns word_gen })
										     std_gen
generateWord word_gen std_gen = let (rand1,gen1) = next $ snd $ next std_gen
				    the_word_patterns = word_patterns word_gen
				    the_word_pattern = the_word_patterns !! (rand1 `mod` (length the_word_patterns))
				    (rand2,gen2) = next $ snd $ next gen1
				    next_soft = softs word_gen !! (rand2 `mod` (length $ softs word_gen))
				    next_hard = hards word_gen !! (rand2 `mod` (length $ hards word_gen))
				    in case head the_word_pattern of
								  Left Hard -> next_hard
								  Left Soft -> next_soft
								  Right str -> str
					   ++ generateWord (word_gen { word_patterns=[tail the_word_pattern] }) gen2

sample_word_generator :: WordGenerator
sample_word_generator = WordGenerator {
				       hards=["k","sh","t","n","ckt","kt","cht","tch"],
				       softs=["enina","iri","ili","ara","ala"],
				       word_patterns=[[Left Hard,Left Soft,Right " ",Left Hard,Left Soft,Left Hard],
						      [Left Hard,Left Soft,Right " ",Left Hard,Left Soft,Left Hard,Left Soft,Left Hard],
						      [Left Soft,Left Soft,Right "-",Left Soft,Left Soft]] 
				      }
