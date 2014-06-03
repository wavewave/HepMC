module HEP.Parser.HepMC where

type Version = String

data EventBlock = EventBlock Version [ Event ]

data MomentumUnit = GeV | MeV 
                  deriving (Show)

data LengthUnit = MM | CM
                deriving (Show)

{- data Line = E GenEvent 
          | N NamedWeight
          | U MomentumPositionUnit
          | C GenXSec
          | H HeavyIon
          | F PdfInfo
          | V GenVertex
          | P GenParticle
-}

data Event = Event { genEventInfo :: GenEvent
                   , eventHeader :: EventHeader 
                   , vertices :: [GenVertex] 
                   }
           deriving Show

data GenEvent = GenEvent { eventNumber :: Int
                         , numMultiparticleInteractions :: Int
                         , eventScale :: Double
                         , alphaQCD :: Double
                         , alphaQED :: Double
                         , signalProcessId :: Int
                         , barcode4SignalProcessVtx :: Int
                         , numVtx :: Int
                         , barcodeBeam1 :: Int
                         , barcodeBeam2 :: Int
                         , randomStateList :: (Int, [Int]) -- ^ (numEntries, randomStateIntergers)
                         , weightList :: (Int, [Double])  -- ^ (numEntries, weights )
                         }
              deriving (Show) 

data EventHeader = EventHeader { weightInfo :: NamedWeight
                               , unitInfo :: MomentumPositionUnit
                               , xsecInfo :: Maybe GenXSec 
                               , heavyIonInfo :: HeavyIon
                               , pdfInfo :: PdfInfo
                               }
                 deriving (Show)
            
data NamedWeight = NamedWeight { numEntries :: Int
                               , weightNames :: [ String ] 
                               }
                 deriving (Show)

data MomentumPositionUnit = MomentumPositionUnit { momentumUnit :: MomentumUnit
                                                 , lengthUnit :: LengthUnit 
                                                 }
                          deriving (Show)

data GenXSec = GenXSec { xsecInPb :: Double 
                       , errorInXsec :: Double }
             deriving (Show)

data HeavyIon = HeavyIon { numHardScattering :: Int
                         , numProjectileParticipants :: Int
                         , numTargetParticipants :: Int
                         , numNNCollisions :: Int
                         , numSpectatorNeutrons :: Int
                         , numSpectatorProtons :: Int
                         , numNNwoundedCollisions :: Int
                         , numNwoundedNCollisions :: Int
                         , numNwoundedNwoundedCollisions :: Int
                         , impactParamCollision :: Double
                         , azimuthalAngleEventPlane :: Double
                         , eccentricityParticipatingNucleonsInTPlane :: Double
                         , inelasticXsecNN :: Double }
              deriving (Show)

data PdfInfo = PdfInfo { flavor1 :: Int
                       , flavor2 :: Int
                       , beamMomFrac1 :: Double
                       , beamMomFrac2 :: Double
                       , scaleQPDF :: Double
                       , xfx1 :: Double
                       , xfx2 :: Double
                       , idLHAPDF1 :: Int
                       , idLHAPDF2 :: Int } 
             deriving (Show) 

data GenVertex = GenVertex { vbarcode :: Int
                           , vid :: Int 
                           , vx :: Double
                           , vy :: Double
                           , vz :: Double
                           , vctau :: Double
                           , numOrphanInPtl :: Int
                           , numOutPtl :: Int
                           , vertexWeights :: (Int, [Double])  -- ^ ( numEntries, weights ) 
                           , particles :: [GenParticle]
                           } 
               deriving (Show)

data GenParticle = GenParticle { pbarcode :: Int
                               , pidPDG :: Int
                               , px :: Double
                               , py :: Double 
                               , pz :: Double
                               , pE :: Double
                               , generatedMass :: Double
                               , statusCode :: Int
                               , polTheta :: Double 
                               , polPhi :: Double
                               , vbarcode4ThisIncoming :: Int
                               , flows :: (Int, [(Int,Int)] ) -- ^ (numEntries, [ (code index, code) ] )
                               }
                 deriving (Show)

