{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Types where

import Data.Text (Text, pack)
import Database.Bolt.Extras.Template (makeNodeLike, makeURelationLike)
import GHC.Generics (Generic)

class ShowNode a where
  showNode :: a -> Text

instance ShowNode Molecule where
  showNode (Molecule id' _ _) = "mol_" <> pack (show id')

instance ShowNode Reaction where
  showNode (Reaction id' _) = "react_" <> pack (show id')

type PathNode = Either Molecule Reaction

data Molecule = Molecule
  { moleculeId :: Int
  , moleculeSmiles :: Text
  , moleculeIupacName :: Text
  }
  deriving (Show, Generic)

data Reaction = Reaction
  { reactionId :: Int
  , reactionName :: Text
  }
  deriving (Show, Generic)

data Catalyst = Catalyst
  { catalystId :: Int
  , catalystSmiles :: Text
  , catalystName :: Maybe Text
  }
  deriving (Show, Generic)

data REAGENT_IN = REAGENT_IN
  deriving (Show, Generic)

data ACCELERATE = ACCELERATE
  { accelerateTemperature :: Float
  , acceleratePressure :: Float
  }
  deriving (Show, Generic)

data PRODUCT_FROM = PRODUCT_FROM
  { productFromAmount :: Float
  }
  deriving (Show, Generic)

data AggregatedReaction = AggregatedReaction
  { reagents :: [Molecule]
  , products :: [(Molecule, PRODUCT_FROM)]
  , reaction :: Reaction
  , catalyst :: Maybe (Catalyst, ACCELERATE)
  }
  deriving (Show)

makeNodeLike ''Molecule
makeNodeLike ''Reaction
makeNodeLike ''Catalyst
makeURelationLike ''REAGENT_IN
makeURelationLike ''PRODUCT_FROM
makeURelationLike ''ACCELERATE