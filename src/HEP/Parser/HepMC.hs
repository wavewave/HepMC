{-# LANGUAGE OverloadedStrings #-}

module HEP.Parser.HepMC where

import Control.Applicative ((<*), (*>))
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Attoparsec.Text
import Data.Text hiding (takeWhile, length)
-- 
import Prelude hiding (takeWhile)

hepmc :: Parser (Text,[(Text,[Int])])
hepmc = do 
    skipSpace 
    ver <- hepmcVersion 
    skipSpace
    blockStart
    skipSpace
    xs <- replicateM 3 event --  many1 event
    -- e <- lineE 
    -- lineN >> lineU >> lineC >> lineF
    -- ns <- many1 vertexParticles 
    return (ver,xs)

event :: Parser (Text,[Int])
event = do 
    e <- lineE 
    lineN >> lineU >> lineC >> lineF
    ns <- many1 vertexParticles 
    return (e,ns)

vertexParticles  :: Parser Int
vertexParticles  = do 
    lineV 
    ps <- many1 lineP
    return (length ps)

    
hepmcVersion :: Parser Version 
hepmcVersion = string "HepMC::Version" >> skipSpace >> takeWhile (not . isSpace)  

blockStart :: Parser ()
blockStart = string "HepMC::IO_GenEvent-START_EVENT_LISTING" >> return ()

lineE :: Parser Text
lineE = char 'E' *> takeWhile (not . isEndOfLine) <* endOfLine

lineN :: Parser Text
lineN = char 'N' *> takeWhile (not . isEndOfLine) <* endOfLine

lineU :: Parser Text
lineU = char 'U' *> takeWhile (not . isEndOfLine) <* endOfLine

lineC :: Parser Text
lineC = char 'C' *> takeWhile (not . isEndOfLine) <* endOfLine

lineF :: Parser Text
lineF = char 'F' *> takeWhile (not . isEndOfLine) <* endOfLine

lineV :: Parser Text
lineV = char 'V' *> takeWhile (not . isEndOfLine) <* endOfLine

lineP :: Parser Text
lineP = char 'P' *> takeWhile (not . isEndOfLine) <* endOfLine




-- return "hepmc parser test IWKIM" 


type Version = Text

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
                               , weightNames :: [ Text ] 
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

