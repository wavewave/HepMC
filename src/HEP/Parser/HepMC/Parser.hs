{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Parser.HepMC.Parser where

import           Control.Applicative
import           Control.Monad                    (replicateM)
import           Control.Monad.Trans.State.Strict
import           Data.Attoparsec.Text
import           Data.Char                        (isSpace)
import           Data.Maybe                       (isJust)
import           Data.Monoid                      (All (..))
import           Data.Text                        (Text)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import           Pipes.Text.IO                    (fromHandle)
import           System.IO                        (Handle)
--
import           HEP.Parser.HepMC.Type
--
-- import           Debug.Trace
import           Prelude                          hiding (takeWhile)

hepmcEvent :: MonadIO m => Handle -> Producer GenEvent m ()
hepmcEvent hin = (lift . evStr) hin >>= parseEvent
  where evStr = execStateT (PA.parse hepmcHeader) . fromHandle

parseEvent :: Monad m => Producer Text m () -> Producer GenEvent m ()
parseEvent s = do (r, s') <- lift $ runStateT (PA.parse event) s
                  case r of Just (Right ev) -> yield ev >> parseEvent s'
                            _               -> return ()

hepmcHeader :: Parser Version
hepmcHeader = do skipSpace
                 ver <- hepmcVersion
                 skipSpace
                 blockStart
                 skipSpace
                 return ver

event :: Parser GenEvent
event = lineE
        <*> header (EventHeader Nothing Nothing Nothing Nothing Nothing)
        <*> many1 vertexParticles

header :: EventHeader -> Parser EventHeader
header h@EventHeader {..} =
    if isSaturated
    then return h
    else do s <- satisfy (inClass "NUCHF")
            case s of
              'N' -> lineN' >>= \r -> header (h { mWeightInfo = Just r })
              'U' -> lineU' >>= \r -> header (h { mUnitInfo = Just r })
              'C' -> lineC' >>= \r -> header (h { mXsecInfo = Just r })
              'H' -> lineH' >>= \r -> header (h { mHeavyIonInfo = Just r })
              'F' -> lineF' >>= \r -> header (h { mPdfInfo = Just r })
              _   -> return h
         <|> return h
  where isSaturated = (getAll . mconcat . map All) [ isJust mWeightInfo
                                                   , isJust mUnitInfo
                                                   , isJust mXsecInfo
                                                   , isJust mHeavyIonInfo
                                                   , isJust mPdfInfo
                                                   ]

vertexParticles  :: Parser GenVertex
vertexParticles  = lineV <*> many1 lineP

hepmcVersion :: Parser Version
hepmcVersion = string "HepMC::Version" >> skipSpace >> takeWhile (not . isSpace)

blockStart :: Parser ()
blockStart = void (string "HepMC::IO_GenEvent-START_EVENT_LISTING")

lineE :: Parser (EventHeader -> [GenVertex] -> GenEvent)
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
           wgtlst <- if wgtnum > 0
                     then skipSpace >> replicateM wgtnum double
                     else return []
           -- skipSpace
           skipWhile (not . isEndOfLine )
           endOfLine
           return (\h vs -> GenEvent { eventNumber                  = evnum
                                     , numMultiparticleInteractions = nmint
                                     , eventScale                   = esc
                                     , alphaQCD                     = aqcd
                                     , alphaQED                     = aqed
                                     , signalProcessId              = sid
                                     , barcode4SignalProcessVtx     = bcd
                                     , numVtx                       = nvtx
                                     , barcodeBeam1                 = bcdbm1
                                     , barcodeBeam2                 = bcdbm2
                                     , randomStateList              = (randomstnum,randomstlst)
                                     , weightList                   = (wgtnum,wgtlst)
                                     , eventHeader                  = h
                                     , vertices                     = vs
                                     })

-- | parser for named weight header line (without N)
lineN' :: Parser NamedWeight
lineN' = do skipSpace
            n <- decimal
            skipSpace
            strs <- replicateM n (skipSpace *> char '"' *> takeWhile (not . (== '"') ) <* char '"')
            skipWhile (not . isEndOfLine )
            endOfLine
            return NamedWeight { numEntries = n, weightNames = strs }

lineU' :: Parser MomentumPositionUnit
lineU' = do skipSpace
            mom <- (string "GEV" >> return GeV) <|> (string "MEV" >> return MeV)
            skipSpace
            len <- (string "MM" >> return MM) <|> (string "CM" >> return CM)
            skipWhile (not . isEndOfLine)
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
            epangle <- double <* skipSpace
            ecc <- double <* skipSpace
            inelstcxsec <- double
            skipWhile (not . isEndOfLine) >> endOfLine
            return HeavyIon { numHardScattering             = nhsc
                            , numProjectileParticipants     = npp
                            , numTargetParticipants         = ntp
                            , numNNCollisions               = nnn
                            , numSpectatorNeutrons          = nsn
                            , numSpectatorProtons           = nsp
                            , numNNwoundedCollisions        = nnnw
                            , numNwoundedNCollisions        = nnwn
                            , numNwoundedNwoundedCollisions = nnwnw
                            , impactParamCollision          = impct
                            , eventPlaneAngle               = epangle
                            , eccentricity                  = ecc
                            , inelasticXsecNN               = inelstcxsec
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
            return PdfInfo { flavor1      = f1
                           , flavor2      = f2
                           , beamMomFrac1 = bx1
                           , beamMomFrac2 = bx2
                           , scaleQPDF    = sqpdf
                           , xfx1         = xfx1'
                           , xfx2         = xfx2'
                           , idLHAPDF1    = id1
                           , idLHAPDF2    = id2
                           }

lineV :: Parser ([GenParticle] -> GenVertex)
lineV = do char 'V' >> skipSpace
           vbcd <- signed decimal <* skipSpace
           vid' <- signed decimal <* skipSpace
           vx' <- double <* skipSpace
           vy' <- double <* skipSpace
           vz' <- double <* skipSpace
           vctau' <- double <* skipSpace
           norphans <- decimal <* skipSpace
           nouts <- decimal <* skipSpace
           nwgts <- decimal
           wgts <- if nwgts > 0
                   then replicateM nwgts (skipSpace *> double)
                   else return []
           skipWhile (not . isEndOfLine) >> endOfLine
           return (\ps -> GenVertex { vbarcode       = vbcd
                                    , vid            = vid'
                                    , vx             = vx'
                                    , vy             = vy'
                                    , vz             = vz'
                                    , vctau          = vctau'
                                    , numOrphanInPtl = norphans
                                    , numOutPtl      = nouts
                                    , vertexWeights  = (nwgts, wgts)
                                    , particles      = ps
                                    })

lineP :: Parser GenParticle
lineP = do char 'P' >> skipSpace
           pbcd <- decimal <* skipSpace
           pid' <- signed decimal <* skipSpace
           px' <- double <* skipSpace
           py' <- double <* skipSpace
           pz' <- double <* skipSpace
           pE' <- double <* skipSpace
           gmass <- double <* skipSpace
           scode <- decimal <* skipSpace
           polTh <- double <* skipSpace
           polPh <- double <* skipSpace
           vbcd <- signed decimal <* skipSpace
           nflows <- decimal
           flows' <- if nflows > 0
                     then replicateM nflows ((,) <$> (skipSpace *> signed decimal <* skipSpace) <*> signed decimal)
                     else return []
           skipWhile (not . isEndOfLine) <* endOfLine
           return GenParticle { pbarcode              = pbcd
                              , pidPDG                = pid'
                              , px                    = px'
                              , py                    = py'
                              , pz                    = pz'
                              , pE                    = pE'
                              , generatedMass         = gmass
                              , statusCode            = scode
                              , polTheta              = polTh
                              , polPhi                = polPh
                              , vbarcode4ThisIncoming = vbcd
                              , flows                 = (nflows, flows')
                              }
