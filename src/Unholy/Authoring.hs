{-# LANGUAGE ExistentialQuantification #-}
module Unholy.Authoring
  ( Component(..), SomeComponent(..), Field(..), FieldKind(..)
  , setComponent, removeComponent, readComponents, validateComponents
  , numberField, textField, boolField, entityField ) where

import Unholy.Types
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Unholy.ECS as ECS
import Control.Monad (foldM,unless,forM_)
import Data.List (nub)

data FieldKind = NumberField Float Float | TextField | ToggleField | EntityField deriving (Eq,Show)
data Field = Field { fieldKey :: String, fieldLabel :: String, fieldKind :: FieldKind } deriving (Eq,Show)
data Component a = Component
  { componentKey :: String, componentDefault :: a, componentFields :: [Field]
  , encodeComponent :: a -> Fields, decodeComponent :: Fields -> Either String a }
data SomeComponent = forall a. SomeComponent (Component a)

setComponent :: Component a -> Entity -> a -> World -> World
setComponent definition e value w = w {authored=IM.alter (Just . update . maybe M.empty id) e (authored w)}
  where update values=M.insert (componentKey definition) (M.union (encodeComponent definition value) (M.findWithDefault M.empty (componentKey definition) values)) values
removeComponent :: String -> Entity -> World -> World
removeComponent key e w=w {authored=IM.update (\values->let remaining=M.delete key values in if M.null remaining then Nothing else Just remaining) e (authored w)}

readComponents :: Component a -> World -> Either String (ECS.Components a)
readComponents definition w=foldM decode ECS.empty (IM.toAscList (authored w))
  where decode values (e,entries)=case M.lookup (componentKey definition) entries of
          Nothing->Right values
          Just fields->case validateFields definition fields >> decodeComponent definition fields of
            Left err->Left ("Entity "++show e++" / "++componentKey definition++": "++err)
            Right value->Right (ECS.insert e value values)

validateComponents :: [SomeComponent] -> World -> Either String ()
validateComponents definitions w=do
  let keys=[componentKey d | SomeComponent d<-definitions]
  unless (all (not.null) keys && length keys==length (nub keys)) (Left "Duplicate or empty authored component registration")
  forM_ definitions $ \(SomeComponent definition)->do
    let fields=componentFields definition; names_ = map fieldKey fields
    unless (all (not.null) names_ && length names_==length (nub names_)) (Left ("Duplicate field registration: "++componentKey definition))
    _<-readComponents definition w
    pure ()

validateFields :: Component a -> Fields -> Either String ()
validateFields definition values=forM_ (componentFields definition) $ \description->
  let bad=Left ("Invalid or missing field: "++fieldKey description)
  in case (fieldKind description,M.lookup (fieldKey description) values) of
    (NumberField lo hi,Just (FloatValue n)) | finite lo && finite hi && lo<=hi && finite n && n>=lo && n<=hi->Right ()
    (TextField,Just (TextValue _))->Right ()
    (ToggleField,Just (BoolValue _))->Right ()
    (EntityField,Just (EntityValue _))->Right ()
    _->bad

numberField :: String -> Fields -> Either String Float
numberField key values=case M.lookup key values of Just (FloatValue value)->Right value;_->Left ("Expected number: "++key)
textField :: String -> Fields -> Either String String
textField key values=case M.lookup key values of Just (TextValue value)->Right value;_->Left ("Expected text: "++key)
boolField :: String -> Fields -> Either String Bool
boolField key values=case M.lookup key values of Just (BoolValue value)->Right value;_->Left ("Expected boolean: "++key)
entityField :: String -> Fields -> Either String (Maybe Entity)
entityField key values=case M.lookup key values of Just (EntityValue value)->Right value;_->Left ("Expected entity reference: "++key)
