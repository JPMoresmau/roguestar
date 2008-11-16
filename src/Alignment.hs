module Alignment
    (Alignment,
     MoralAlignment(..),
     EthicalAlignment(..),
     alignments,
     alignmentMoralPotency,
     alignmentEthicalPotency,
     alignmentPotency)
    where

data MoralAlignment = Lawful | Neutral | Chaotic | Evil deriving (Eq,Read,Show,Ord)
data EthicalAlignment = Strategic | Tactical | Diplomatic | Indifferent deriving (Eq,Read,Show,Ord)
type Alignment = (MoralAlignment,EthicalAlignment)

alignments :: [Alignment]
alignments = 
    do moral <- [Lawful,Neutral,Chaotic,Evil]
       ethical <- [Strategic,Tactical,Diplomatic,Indifferent]
       return (moral,ethical)

alignmentMoralPotency :: MoralAlignment -> Integer
alignmentMoralPotency Lawful = 6
alignmentMoralPotency Chaotic = 3
alignmentMoralPotency Neutral = 1
alignmentMoralPotency Evil = 10

alignmentEthicalPotency :: EthicalAlignment -> Integer
alignmentEthicalPotency Strategic = 7
alignmentEthicalPotency Tactical = 2
alignmentEthicalPotency Diplomatic = 4
alignmentEthicalPotency Indifferent = 10

alignmentPotency :: Alignment -> Integer
alignmentPotency (moral,ethical) = alignmentMoralPotency moral * alignmentEthicalPotency ethical
