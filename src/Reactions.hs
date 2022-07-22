{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reactions
    ( putReactions
    ) where

import Control.Exception (bracket)
import Data.Text (Text, pack)
import Control.Monad.State (execState, modify, State)
import Data.Default
import Database.Bolt
import Database.Bolt.Extras (NodeLike (..), URelationLike (..))
import Database.Bolt.Extras.Graph
import Types
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Functor ((<&>))

boltCfg :: BoltCfg
boltCfg = def {host = "localhost", user = "neo4j", password = "12345"}

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

-- create reaction
addReagentToReaction :: Text -> Molecule -> State GraphPutRequest ()
addReagentToReaction reactionLabel m = do
  let moleculeLabel = showNode m
  modify $ addNode moleculeLabel (MergeN . toNode $ m)
  modify $ addRelation moleculeLabel reactionLabel (MergeR . toURelation $ REAGENT_IN)

addProductToReaction :: Text -> (Molecule, PRODUCT_FROM) -> State GraphPutRequest ()
addProductToReaction reactionLabel (m, p) = do
  let moleculeLabel = showNode m
  modify $ addNode moleculeLabel (MergeN . toNode $ m)
  modify $ addRelation reactionLabel moleculeLabel (MergeR . toURelation $ p)

addCatalystToReaction :: Text -> Maybe (Catalyst, ACCELERATE) -> State GraphPutRequest ()
addCatalystToReaction _ Nothing = pure ()
addCatalystToReaction reactionLabel (Just (catalyst', accelerate')) = do
  let catalystLabel = "catalyst"
  modify $ addNode catalystLabel (MergeN . toNode $ catalyst')
  modify $ addRelation catalystLabel reactionLabel (MergeR . toURelation $ accelerate')

putReactionReq :: AggregatedReaction -> GraphPutRequest
putReactionReq (AggregatedReaction reagents' products' reaction' catalyst') = flip execState emptyGraph $ do
  let reactionLabel = showNode reaction'
  mapM_ (addReagentToReaction reactionLabel) reagents'
  mapM_ (addProductToReaction reactionLabel) products'
  addCatalystToReaction reactionLabel catalyst'
  modify $ addNode reactionLabel (MergeN . toNode $ reaction')

-- get reaction
getReactionReq :: Int -> BoltActionT IO [Path]
getReactionReq relId = do
  result <- queryP "MATCH p=(react:Reaction{reactionId:$reactId})-[]-() RETURN p" (props ["reactId" =: relId])
  mapM (`at` "p") result

checkURelations :: [URelationship] -> Text -> Bool
checkURelations rels lbl = case  listToMaybe rels of
  Just l -> urelType l == lbl
  Nothing -> False

filterPathByLabel :: [Path] -> Text -> [Path]
filterPathByLabel p lbl = filter (\(Path _ rels _) -> checkURelations rels lbl) p

getSecondNode :: NodeLike a => [Node] -> Maybe a
getSecondNode (_ : n : _) = Just $ fromNode n
getSecondNode _ = Nothing

getURelation :: URelationLike a => [URelationship] -> Maybe a
getURelation (r : _) = Just $ fromURelation r
getURelation _ = Nothing

getPair :: (NodeLike a, URelationLike b) => Path -> Maybe (a, b)
getPair (Path nodes rels _) = do
  node <- getSecondNode nodes
  rel <- getURelation rels
  return (node, rel)

extractAgregatedReaction :: [Path] -> Maybe AggregatedReaction
extractAgregatedReaction paths = do
  path <- listToMaybe paths
  reaction' <- listToMaybe $ pathNodes path
  return $
    AggregatedReaction
      { reagents = mapMaybe (getSecondNode . pathNodes) (filterPathByLabel paths "REAGENT_IN")
      , products = mapMaybe getPair (filterPathByLabel paths "PRODUCT_FROM")
      , reaction = fromNode reaction'
      , catalyst = listToMaybe $ mapMaybe getPair (filterPathByLabel paths "ACCELERATE")
      }

-- shortest path
nodeToPathNode :: Node -> Maybe PathNode
nodeToPathNode n = case labels n of
  ["Molecule"] -> Just $ Left $ fromNode n
  ["Reaction"] -> Just $ Right $ fromNode n
  _ -> Nothing

showPath :: [PathNode] -> Text
showPath paths = fold $ intersperse "->" $ map (either showNode showNode) paths

extractNodePath :: [Path] -> Maybe [PathNode]
extractNodePath paths = do
  path' <- listToMaybe paths
  return $ mapMaybe nodeToPathNode $ pathNodes path'

getShortestPathReq :: Int -> Int -> BoltActionT IO [Path]
getShortestPathReq startId endId = do
  result <-
    queryP
      "MATCH (m1:Molecule{moleculeId:$startId}), (m2:Molecule{moleculeId:$endId}), p=shortestPath((m1)-[*..10]->(m2)) RETURN p"
      (props ["startId" =: startId, "endId" =: endId])
  mapM (`at` "p") result

-- seed data
genNameFromId :: Text -> Int -> Text
genNameFromId prefix id' = prefix <> pack (show id')

genMolecule :: Int -> Molecule
genMolecule mId = Molecule mId (genNameFromId "smiles_" mId) (genNameFromId "iupac_" mId)

genCatalyst :: Int -> Catalyst
genCatalyst cId = Catalyst cId (genNameFromId "smiles_" cId) (Just $ genNameFromId "iupac_" cId)

genAggregatedReaction :: Int -> AggregatedReaction
genAggregatedReaction rId =
  AggregatedReaction
    { reagents = [genMolecule rId, genMolecule $ rId + 1]
    , products = [(genMolecule $ rId + 2, PRODUCT_FROM 2), (genMolecule $ rId + 3, PRODUCT_FROM 3)]
    , reaction = Reaction rId (genNameFromId "reaction_" rId)
    , catalyst = Just (genCatalyst rId, ACCELERATE 100.1 3.1)
    }

genSimpleAggregatedReaction :: Int -> Int -> Int -> AggregatedReaction
genSimpleAggregatedReaction reactId rId pId =
  AggregatedReaction
    { reagents = [genMolecule rId]
    , products = [(genMolecule pId, PRODUCT_FROM 2)]
    , reaction = Reaction reactId (genNameFromId "reaction_" rId)
    , catalyst = Nothing
    }

-- main functions to communicate with db
getReaction :: Int -> IO (Maybe AggregatedReaction)
getReaction reactionId' = runQueryDB $ getReactionReq reactionId' <&> extractAgregatedReaction

putReaction :: AggregatedReaction -> IO ()
putReaction reaction' = runQueryDB $ makeRequest @PutRequest [] (putReactionReq reaction') >> pure ()

getShortestPath :: Int -> Int -> IO (Maybe [PathNode])
getShortestPath startId endId = runQueryDB $ getShortestPathReq startId endId <&> extractNodePath

printPath :: Maybe [PathNode] -> IO ()
printPath p = print $ p <&> showPath

putReactions :: IO ()
putReactions = do
  mapM_ (putReaction . genAggregatedReaction) [1, 4 .. 80]
  r <- getReaction 7
  print r

  -- no path
  p <- getShortestPath 1 5
  printPath p

  -- long path
  p1 <- getShortestPath 1 10
  printPath p1

  -- create shortcut
  putReaction $ genSimpleAggregatedReaction 2 1 10

  -- short path
  p2 <- getShortestPath 1 10
  printPath p2

