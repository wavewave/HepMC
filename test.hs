{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import           Control.Category ((>>>))
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Text as T

import HEP.Parser.HepMC.Type 


p1 = GenParticle { pbarcode = 1
                                                    , pidPDG = 2212
                                                    , px = 0 
                                                    , py = 0 
                                                    , pz = 4.00e+03
                                                    , pE = 4.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 3
                                                    , polTheta = 0
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = -1
                                                    , flows = (0,[]) }
                                                    

main = do
    -- B.putStrLn $ toByteString (buildParticle p1)
    -- B.putStrLn $ toByteString (buildVertex testvertex1)
    -- B.putStrLn $ toByteString (buildHeader testheader)
    B.putStrLn $ toByteString (buildEvent testevent)

buildParticle :: GenParticle -> Builder
buildParticle GenParticle {..} = fromText "P " 
                                 <> fromShow pbarcode <> space 
                                 <> fromShow px <> space
                                 <> fromShow py <> space 
                                 <> fromShow pz <> space 
                                 <> fromShow pE <> space 
                                 <> fromShow generatedMass <> space
                                 <> fromShow statusCode <> space
                                 <> fromShow polTheta <> space
                                 <> fromShow polPhi <> space
                                 <> fromShow vbarcode4ThisIncoming <> space 
                                 <> (let (n,lst) = flows 
                                     in fromShow n <> space
                                        <> mconcat (map (\x -> fromShow x <> space) lst))

buildVertex GenVertex {..} = 
    fromText "V "
    <> fromShow vbarcode <> space
    <> fromShow vid <> space 
    <> fromShow vx <> space 
    <> fromShow vy <> space
    <> fromShow vz <> space
    <> fromShow vctau <> space 
    <> fromShow numOrphanInPtl <> space
    <> fromShow numOutPtl <> space
    <> (let (n,lst) = vertexWeights
        in fromShow n <> space
           <> mconcat (map (\x -> fromShow x <> space) lst))
    <> fromText "\n"
    <> mconcat (map (\x -> buildParticle x <> fromText "\n") particles)

  
buildHeader :: EventHeader -> Builder
buildHeader EventHeader {..} = 
    maybe mempty buildWeightInfo mWeightInfo
    <> maybe mempty (buildUnitInfo >>> (<> fromText "\n")) mUnitInfo
    <> maybe mempty (buildXsecInfo >>> (<> fromText "\n")) mXsecInfo
    <> maybe mempty (buildHeavyIonInfo >>> (<> fromText "\n")) mHeavyIonInfo
    <> maybe mempty (buildPdfInfo >>> (<> fromText "\n"))  mPdfInfo
  where buildWeightInfo NamedWeight {..} = undefined -- postpone 
        buildUnitInfo MomentumPositionUnit {..} = fromText "U " <> fromText (case momentumUnit of GeV -> "GEV" ; MeV -> "MEV")
                                                                <> space 
                                                                <> fromText (case lengthUnit of MM -> "MM" ; CM -> "CM")
        buildXsecInfo GenXSec {..} = fromText "C " <> fromShow xsecInPb <> space <> fromShow errorInXsec
        buildHeavyIonInfo _ = undefined  -- postpone
        buildPdfInfo _ = undefined -- postpone

space = fromText " " 
newline = fromText "\n"

buildEvent :: GenEvent -> Builder
buildEvent GenEvent {..} = fromText "HepMC::Version 2.06.09\nHepMC::IO_GenEvent-START_EVENT_LISTING\n"
                           <> fromText "E " 
                           <> fromShow eventNumber <> space
                           <> fromShow numMultiparticleInteractions <> space
                           <> fromShow eventScale <> space 
                           <> fromShow alphaQCD <> space
                           <> fromShow alphaQED <> space 
                           <> fromShow signalProcessId <> space
                           <> fromShow barcode4SignalProcessVtx <> space
                           <> fromShow numVtx <> space
                           <> fromShow barcodeBeam1 <> space
                           <> fromShow barcodeBeam2 <> space
                           <> (let (n1,lst1) = randomStateList 
                               in fromShow n1 <> space <> mconcat (map (fromShow >>> (<> space)) lst1)) 
                           <> (let (n2,lst2) = weightList
                               in fromShow n2 <> space <> mconcat (map (fromShow >>> (<> space)) lst2))
                           <> newline
                           <> buildHeader eventHeader
                           <> F.foldMap buildVertex vertices
                         

testevent = GenEvent { eventNumber = 1
                     , numMultiparticleInteractions = 1
                     , eventScale = 0
                     , alphaQCD = 0
                     , alphaQED = 0
                     , signalProcessId = 0 
                     , barcode4SignalProcessVtx= -3  
                     , numVtx = 3
                     , barcodeBeam1 = 1
                     , barcodeBeam2 = 4
                     , randomStateList = (0, []) 
                     , weightList = (0, [] )
                     , eventHeader = testheader
                     , vertices = testvertices }
                     
testheader = EventHeader { mWeightInfo = Nothing
                         , mUnitInfo = Just (MomentumPositionUnit GeV MM)
                         , mXsecInfo = Just (GenXSec 0.0 0.0) 
                         , mHeavyIonInfo = Nothing
                         , mPdfInfo = Nothing }

testvertices = [ testvertex1, testvertex2, testvertex3 ]

testvertex1 = GenVertex { vbarcode = -1
                        , vid = 0
                        , vx = 0
                        , vy = 0 
                        , vz = 0 
                        , vctau = 0 
                        , numOrphanInPtl = 1
                        , numOutPtl = 2
                        , vertexWeights = (0,[])
                        , particles = [ GenParticle { pbarcode = 1
                                                    , pidPDG = 2212
                                                    , px = 0 
                                                    , py = 0 
                                                    , pz = 4.00e+03
                                                    , pE = 4.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 3
                                                    , polTheta = 0
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = -1
                                                    , flows = (0,[])
                                                    }
                                      , GenParticle { pbarcode = 2
                                                    , pidPDG = 1000022
                                                    , px = 0 
                                                    , py = 0
                                                    , pz = 2.00e+03
                                                    , pE = 2.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0 
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[]) 
                                                    } 

                                      , GenParticle { pbarcode = 3
                                                    , pidPDG = 1
                                                    , px = 0 
                                                    , py = 0
                                                    , pz = 2.00e+03
                                                    , pE = 2.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 3
                                                    , polTheta = 0 
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = -3
                                                    , flows = (0,[]) 
                                                    } 
                                      ]
                            }

testvertex2 = GenVertex { vbarcode = -2
                        , vid = 0
                        , vx = 0
                        , vy = 0 
                        , vz = 0 
                        , vctau = 0 
                        , numOrphanInPtl = 1
                        , numOutPtl = 2
                        , vertexWeights = (0,[])
                        , particles = [ GenParticle { pbarcode = 4
                                                    , pidPDG = 2212
                                                    , px = 0 
                                                    , py = 0 
                                                    , pz = -4.00e+03
                                                    , pE = 4.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 3
                                                    , polTheta = 0
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = -1
                                                    , flows = (0,[])
                                                    }
                                      , GenParticle { pbarcode = 5
                                                    , pidPDG = 1000022
                                                    , px = 0 
                                                    , py = 0
                                                    , pz = -2.00e+03
                                                    , pE = 2.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0 
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[]) 
                                                    } 

                                      , GenParticle { pbarcode = 6
                                                    , pidPDG = 1
                                                    , px = 0 
                                                    , py = 0
                                                    , pz = 2.00e+03
                                                    , pE = 2.00e+03
                                                    , generatedMass = 0.0
                                                    , statusCode = 3
                                                    , polTheta = 0 
                                                    , polPhi = 0 
                                                    , vbarcode4ThisIncoming = -3
                                                    , flows = (0,[]) 
                                                    } 
                                      ]
                            }

testvertex3 = GenVertex { vbarcode = -3
                        , vid = 0, vx = 0, vy = 0, vz = 0, vctau = 0 
                        , numOrphanInPtl = 0, numOutPtl = 2
                        , vertexWeights = (0,[])
                        , particles = [ GenParticle { pbarcode = 7
                                                    , pidPDG = 11
                                                    , px = 1.00e+03, py = 1.00e+03, pz = 0, pE = 1.4142e+03, generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0, polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[])
                                                    }
                                      , GenParticle { pbarcode = 8
                                                    , pidPDG = -11
                                                    , px = -1.00e+03, py = -1.00e+03, pz = 0, pE = 1.4142e+03, generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0, polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[]) 
                                                    } 
                                      ]
                            }
