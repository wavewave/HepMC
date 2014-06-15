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
    -- n <- lineN
    -- u <- lineU 
    -- lineC >> lineF
    -- ns <- many1 vertexParticles 
    -- return e -- (e,n,u,ns)

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
lineN' = do -- char 'N' 
            skipSpace
            n <- decimal
            skipSpace
            strs <- replicateM n (skipSpace *> char '"' *> takeWhile (not . (== '"') ) <* char '"' )
            skipWhile (not . isEndOfLine )
            endOfLine 
            return NamedWeight { numEntries = n, weightNames = strs }
          

 
lineU' :: Parser MomentumPositionUnit
lineU' = do -- char 'U'
            skipSpace
            mom <- (try (string "GEV" >> return GeV) <|> (string "MEV" >> return MeV))
            skipSpace
            len <- (try (string "MM" >> return MM) <|> (string "CM" >> return CM))
            skipWhile (not . isEndOfLine )
            endOfLine 
            return (MomentumPositionUnit mom len)

lineC' :: Parser Text
lineC' = -- char 'C' *> 
         takeWhile (not . isEndOfLine) <* endOfLine

lineH' :: Parser Text
lineH' = takeWhile (not . isEndOfLine) <* endOfLine


lineF' :: Parser Text
lineF' = -- char 'F' *> 
         takeWhile (not . isEndOfLine) <* endOfLine

lineV :: Parser Text
lineV = char 'V' *> 
         takeWhile (not . isEndOfLine) <* endOfLine

lineP :: Parser Text
lineP = char 'P' *> 
         takeWhile (not . isEndOfLine) <* endOfLine




