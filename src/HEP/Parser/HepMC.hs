{-# LANGUAGE OverloadedStrings #-}

module HEP.Parser.HepMC where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Attoparsec.Text
import Data.Text hiding (takeWhile, length)
-- 
import Prelude hiding (takeWhile)

hepmcHeader :: Parser Version
hepmcHeader = do 
    skipSpace 
    ver <- hepmcVersion 
    skipSpace
    blockStart
    skipSpace
    return ver

  
event :: Parser (GenEvent,NamedWeight,MomentumPositionUnit,[Int])
event = do 
    e <- lineE
    n <- lineN
    u <- lineU 
    lineC >> lineF
    ns <- many1 vertexParticles 
    return (e,n,u,ns)

vertexParticles  :: Parser Int
vertexParticles  = do 
    lineV 
    ps <- many1 lineP
    return (length ps)

    
hepmcVersion :: Parser Version 
hepmcVersion = string "HepMC::Version" >> skipSpace >> takeWhile (not . isSpace)  

blockStart :: Parser ()
blockStart = string "HepMC::IO_GenEvent-START_EVENT_LISTING" >> return ()

lineE :: Parser GenEvent
lineE = do char 'E' 
           skipSpace 
           evnum <- decimal
           skipSpace
           nmint <- signed decimal
           skipSpace
           esc <- double
           skipSpace
           aqcd <- double
           skipSpace
           aqed <- double
           skipSpace
           sid <- decimal
           skipSpace
           bcd <- signed decimal
           skipSpace
           nvtx <- decimal
           skipSpace
           bcdbm1 <- decimal
           skipSpace
           bcdbm2 <- decimal
           skipSpace
           randomstnum <- decimal
           skipSpace
           randomstlst <- replicateM randomstnum decimal
           skipSpace
           wgtnum <- decimal
           skipSpace
           wgtlst <- replicateM wgtnum double 
           -- skipSpace
           skipWhile (not . isEndOfLine )
           endOfLine
           return GenEvent { eventNumber = evnum
                           , numMultiparticleInteractions = nmint 
                           , eventScale = esc
                           , alphaQCD = aqcd
                           , alphaQED = aqed
                           , signalProcessId = sid
                           , barcode4SignalProcessVtx = bcd
                           , numVtx = nvtx
                           , barcodeBeam1 = bcdbm1
                           , barcodeBeam2 = bcdbm2
                           , randomStateList = (randomstnum,randomstlst)
                           , weightList = (wgtnum,wgtlst) 
                           }

lineN :: Parser NamedWeight
lineN = do char 'N' 
           skipSpace
           n <- decimal
           skipSpace
           strs <- replicateM n (skipSpace *> char '"' *> takeWhile (not . (== '"') ) <* char '"' )
           skipWhile (not . isEndOfLine )
           endOfLine 
           return NamedWeight { numEntries = n, weightNames = strs }
          
 
lineU :: Parser MomentumPositionUnit
lineU = do char 'U'
           skipSpace
           mom <- (try (string "GEV" >> return GeV) <|> (string "MEV" >> return MeV))
           skipSpace
           len <- (try (string "MM" >> return MM) <|> (string "CM" >> return CM))
           skipWhile (not . isEndOfLine )
           endOfLine 
           return (MomentumPositionUnit mom len)
--  *> takeWhile (not . isEndOfLine) <* endOfLine

lineC :: Parser Text
lineC = char 'C' *> takeWhile (not . isEndOfLine) <* endOfLine

lineF :: Parser Text
lineF = char 'F' *> takeWhile (not . isEndOfLine) <* endOfLine

lineV :: Parser Text
lineV = char 'V' *> takeWhile (not . isEndOfLine) <* endOfLine

lineP :: Parser Text
lineP = char 'P' *> takeWhile (not . isEndOfLine) <* endOfLine




type Version = Text

data EventBlock = EventBlock Version [ Event ]

data MomentumUnit = GeV | MeV 
                  deriving (Show)

data LengthUnit = MM | CM
                deriving (Show)


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

