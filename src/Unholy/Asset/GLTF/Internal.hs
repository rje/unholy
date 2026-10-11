-- Constructors stay internal: only the validated loader creates these values.
module Unholy.Asset.GLTF.Internal where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word16)
import Linear
import Unholy.Renderer.GLTF (MaterialInfo)

type Bounds = (V3 Float, V3 Float)

data Node = Node
  { nodeParent :: !Int,
    nodeMatrix :: !(M44 Float),
    nodeTranslation :: !(V3 Float),
    nodeRotation :: !(Quaternion Float),
    nodeScale :: !(V3 Float),
    matrixNode :: !Bool
  }
  deriving (Show)

data Skin = Skin {skinJoints :: ![Int], inverseBinds :: ![M44 Float]} deriving (Show)

data RigMesh = RigMesh
  { bindVertices :: !(VS.Vector Float),
    triangleIndices :: !(VS.Vector Word16),
    vertexJoints :: !(VS.Vector (V4 Word16)),
    vertexWeights :: !(VS.Vector (V4 Float)),
    meshSkin :: !(Maybe Int),
    meshNode :: !Int,
    meshMaterial :: !(Maybe Int),
    influenceBounds :: !(IM.IntMap Bounds)
  }
  deriving (Show)

data Channel = Channel
  { channelNode :: !Int,
    channelPath :: !String,
    channelStep :: !Bool,
    keyTimes :: !(VS.Vector Float),
    keyValues :: !(VS.Vector (V4 Float))
  }
  deriving (Show)

data Clip = Clip {clipDuration :: !Float, clipChannels :: ![Channel]} deriving (Show)

data Image = ImageFile !FilePath | ImageBytes !String !B.ByteString deriving (Show)

data Rig = Rig
  { rigNodes :: !(V.Vector Node),
    rigOrder :: !(V.Vector Int),
    rigSkins :: !(V.Vector Skin),
    rigMeshes :: ![RigMesh],
    rigClips :: !(M.Map String Clip),
    rigImages :: !(V.Vector Image),
    rigMaterialInfos :: !(IM.IntMap MaterialInfo),
    rigMaterialImages :: !(IM.IntMap [Maybe Int]),
    rigMaterialError :: !(Maybe String)
  }
  deriving (Show)
