{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Parser.HepMC.Parser where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text hiding (takeWhile, length, map)
--
import HEP.Parser.HepMC.Type
--
import Prelude hiding (takeWhile)
import Debug.Trace

hepmcHeader :: Parser Version
hepmcHeader = do 
    skipSpace 
    ver <- hepmcVersion 
    skipSpace
    blockStart
    skipSpace
    return ver

  
event :: Parser GenEvent
event = lineE <*> header (EventHeader Nothing Nothing Nothing Nothing Nothing)
  -- liftA2 lineE (header (EventHeader Nothing Nothing Nothing Nothing Nothing)) vertexParticles 

header :: EventHeader -> Parser EventHeader
header h@EventHeader {..} = 
    if isSaturated 
      then return h
      else try ( do s <- satisfy (inClass "NUCHF")
                    trace (show s) $ do 
                     case s of
                      'N' -> lineN' >>= \r -> header (h { mWeightInfo = Just r })
                      'U' -> lineU' >>= \r -> header (h { mUnitInfo = Just r })
                      'C' -> lineC' >>= \r -> header (h { mXsecInfo = Just r })
                      'H' -> lineH' >>= \r -> header (h { mHeavyIonInfo = Just r })
                      'F' -> lineF' >>= \r -> header (h { mPdfInfo = Just r })
                      _   -> return h
               )
           <|> return h                
  where isSaturated = (getAll . mconcat . map All) [isJust mWeightInfo, isJust mUnitInfo, isJust mXsecInfo, isJust mHeavyIonInfo, isJust mPdfInfo]

vertexParticles  :: Parser Int
vertexParticles  = do 
    lineV 
    ps <- many1 lineP
    return (length ps)

    
hepmcVersion :: Parser Version 
hepmcVersion = string "HepMC::Version" >> skipSpace >> takeWhile (not . isSpace)  

blockStart :: Parser ()
blockStart = string "HepMC::IO_GenEvent-START_EVENT_LISTING" >> return ()

lineE :: Parser (EventHeader -> GenEvent)
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
           wgtlst <- if wgtnum > 0 then skipSpace >> replicateM wgtnum double else return []
           -- skipSpace
           skipWhile (not . isEndOfLine )
           endOfLine
           return (\h -> GenEvent { eventNumber = evnum
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
                                  , eventHeader = h
                                  })

-- | parser for named weight header line (without N)
lineN' :: Parser NamedWeight
lineN' = do skipSpace
            n <- decimal
            skipSpace
            strs <- replicateM n (skipSpace *> char '"' *> takeWhile (not . (== '"') ) <* char '"' )
            skipWhile (not . isEndOfLine )
            endOfLine 
            return NamedWeight { numEntries = n, weightNames = strs }
          

 
lineU' :: Parser MomentumPositionUnit
lineU' = do skipSpace
            mom <- (try (string "GEV" >> return GeV) <|> (string "MEV" >> return MeV))
            skipSpace
            len <- (try (string "MM" >> return MM) <|> (string "CM" >> return CM))
            skipWhile (not . isEndOfLine )
            endOfLine 
            return (MomentumPositionUnit mom len)

lineC' :: Parser GenXSec
lineC' = do skipSpace
            xsec <- double
            skipSpace
            err <- double
            skipWhile (not . isEndOfLine)
            endOfLine
            return (GenXSec xsec err)


lineH' :: Parser HeavyIon
lineH' = do skipSpace
            nhsc <- decimal <* skipSpace
            npp <- decimal <* skipSpace
            ntp <- decimal <* skipSpace
            nnn <- decimal <* skipSpace
            nsn <- decimal <* skipSpace
            nsp <- decimal <* skipSpace
            nnnw <- decimal <* skipSpace
            nnwn <- decimal <* skipSpace
            nnwnw <- decimal<* skipSpace
            impct <- double <* skipSpace
            aziangle <- double <* skipSpace
            eccntrcty <- double <* skipSpace
            inelstcxsec <- double 
            skipWhile (not . isEndOfLine) >> endOfLine
            return HeavyIon { numHardScattering         = nhsc
                            , numProjectileParticipants = npp
                            , numTargetParticipants     = ntp
                            , numNNCollisions           = nnn
                            , numSpectatorNeutrons      = nsn
                            , numSpectatorProtons       = nsp
                            , numNNwoundedCollisions    = nnnw
                            , numNwoundedNCollisions    = nnwn
                            , numNwoundedNwoundedCollisions = nnwnw
                            , impactParamCollision      = impct
                            , azimuthalAngleEventPlane  = aziangle
                            , eccentricityParticipatingNucleonsInTPlane = eccntrcty
                            , inelasticXsecNN           = inelstcxsec
                            }

lineF' :: Parser PdfInfo
lineF' = do skipSpace
            f1 <- decimal <* skipSpace
            f2 <- decimal <* skipSpace
            bx1 <- double <* skipSpace
            bx2 <- double <* skipSpace
            sqpdf <- double <* skipSpace
            xfx1' <- double <* skipSpace
            xfx2' <- double <* skipSpace
            id1 <- signed decimal <* skipSpace
            id2 <- signed decimal
            skipWhile (not . isEndOfLine) >> endOfLine            
            return PdfInfo { flavor1 = f1
                           , flavor2 = f2
                           , beamMomFrac1 = bx1
                           , beamMomFrac2 = bx2
                           , scaleQPDF = sqpdf
                           , xfx1 = xfx1'
                           , xfx2 = xfx2'
                           , idLHAPDF1 = id1
                           , idLHAPDF2 = id2 
                           }


lineV :: Parser Text
lineV = char 'V' *> 
         takeWhile (not . isEndOfLine) <* endOfLine

lineP :: Parser Text
lineP = char 'P' *> 
         takeWhile (not . isEndOfLine) <* endOfLine




