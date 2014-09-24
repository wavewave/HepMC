{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import           Control.Applicative
import           Control.Category ((>>>))
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Text as T
import System.Random.Mersenne
--
import HEP.Parser.HepMC.Type 
import HEP.Parser.HepMC.Builder


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
    let pt = 100
    gen <- newMTGen Nothing
    -- phi <- (2.0 * pi *) <$> random gen :: IO Double
    
    plst <- sequence (replicate 10000 (go gen pt))
    let evts = zipWith testevent [1..] plst
    B.putStrLn $ toByteString (buildHepMC evts)
    -- print phi

    -- B.putStrLn $ toByteString (buildEvent (testevent (3000,1500,220) ))

go gen pt = do
    phi <- (2.0 * pi *) <$> random gen :: IO Double
    -- costh <- (\x -> x * 2.0 - 1.0 ) <$> random gen :: IO Double 
    let costh = 0
    let sinth = sqrt (1 - costh*costh)
        pz = pt * costh / sinth
        px = pt * cos phi
        py = pt * sin phi
    return (px,py,pz)  
   
                         

testevent n (px,py,pz) = GenEvent { eventNumber = n 
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
                     , vertices = testvertices (px,py,pz) }
                     
testheader = EventHeader { mWeightInfo = Nothing
                         , mUnitInfo = Just (MomentumPositionUnit GeV MM)
                         , mXsecInfo = Just (GenXSec 0.0 0.0) 
                         , mHeavyIonInfo = Nothing
                         , mPdfInfo = Nothing }

testvertices (px,py,pz) = [ testvertex1, testvertex2, testvertex3 (px,py,pz) ]

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

testvertex3 (px',py',pz') = GenVertex { vbarcode = -3
                        , vid = 0, vx = 0, vy = 0, vz = 0, vctau = 0 
                        , numOrphanInPtl = 0, numOutPtl = 2
                        , vertexWeights = (0,[])
                        , particles = [ GenParticle { pbarcode = 7
                                                    , pidPDG = 11
                                                    , px = px', py = py', pz = pz', pE = sqrt (px'^2 + py'^2 + pz'^2), generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0, polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[])
                                                    }
                                      , GenParticle { pbarcode = 8
                                                    , pidPDG = -12
                                                    , px = -px', py = -py', pz = -pz', pE = sqrt (px'^2 + py'^2 + pz'^2), generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0, polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[]) 
                                                    } 
                                      ]
                            }
