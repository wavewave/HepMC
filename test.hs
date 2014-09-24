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
import HEP.Parser.LHCOAnalysis.PhysObj
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
                                                    
genEvent :: (Int -> (Double,Double, Double) -> GenEvent) 
            ->  MTGen -> Double -> Double -> IO B.ByteString
genEvent evgen gen pt eta = do
    -- phi <- (2.0 * pi *) <$> random gen :: IO Double    
    plst <- sequence (replicate 10000 (go gen pt eta))
    let evts = zipWith evgen [1..] plst
    return $ toByteString (buildHepMC evts)
{- 
genMuon :: MTGen -> Double -> Double -> IO B.ByteString
genMuon gen pt eta = do
    -- phi <- (2.0 * pi *) <$> random gen :: IO Double    
    plst <- sequence (replicate 10000 (go gen pt eta))
    let evts = zipWith elect [1..] plst
    return $ toByteString (buildHepMC evts)
-}

go :: MTGen -> Double -> Double -> IO (Double,Double,Double)
go gen pt eta = do
    phi <- (2.0 * pi *) <$> random gen :: IO Double
    
    -- costh <- (\x -> x * 2.0 - 1.0 ) <$> random gen :: IO Double 
    let costh = etatocosth eta
    let sinth = sqrt (1 - costh*costh)
        pz = pt * costh / sinth
        px = pt * cos phi
        py = pt * sin phi
    return (px,py,pz)  


mkFilename :: String -> Double -> Double -> FilePath
mkFilename str pt eta = str ++ "_" ++ show pt ++ "_" ++ show eta ++ ".hepmc"

ptetaset = [ (pt,eta) | pt <- [ 10,20,30,40,50,60,70,80,90,100 ] ++ [150,200,250,300] , eta <- [-3.0,-2.5..3.0 ] ]
 
work str evgen gen (pt,eta) = do
    let filename = mkFilename str pt eta 
    B.writeFile filename =<< genEvent evgen gen pt eta

main' = do 
  gen <- newMTGen Nothing
  mapM_  (work "electron" mkelec gen) ptetaset 

main = do 
  gen <- newMTGen Nothing
  mapM_  (work "muon" mkmuon gen) ptetaset 


dummybody :: (Int,Int) -> Int -> (Double,Double,Double) 
          -> ((Int,Int) -> (Double,Double,Double) -> [GenVertex]) -> GenEvent
dummybody (pdg1,pdg2) n (px,py,pz) genvtx = 
    GenEvent { eventNumber = n 
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
             , vertices = genvtx (pdg1,pdg2) (px,py,pz) }


mkelec :: Int -> (Double,Double,Double) -> GenEvent
mkelec n (px,py,pz) = dummybody (11,-12) n (px,py,pz) testvertices
 
mkmuon :: Int -> (Double,Double,Double) -> GenEvent
mkmuon n (px,py,pz) = dummybody (13,-14) n (px,py,pz) testvertices
                     
testheader = EventHeader { mWeightInfo = Nothing
                         , mUnitInfo = Just (MomentumPositionUnit GeV MM)
                         , mXsecInfo = Just (GenXSec 0.0 0.0) 
                         , mHeavyIonInfo = Nothing
                         , mPdfInfo = Nothing }

testvertices (pdg1,pdg2) (px,py,pz) = [ testvertex1, testvertex2, testvertex3 (pdg1,pdg2) (px,py,pz) ]

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

testvertex3 (pdg1,pdg2) (px',py',pz') = GenVertex { vbarcode = -3
                        , vid = 0, vx = 0, vy = 0, vz = 0, vctau = 0 
                        , numOrphanInPtl = 0, numOutPtl = 2
                        , vertexWeights = (0,[])
                        , particles = [ GenParticle { pbarcode = 7
                                                    , pidPDG = pdg1
                                                    , px = px', py = py', pz = pz', pE = sqrt (px'^2 + py'^2 + pz'^2), generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0, polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[])
                                                    }
                                      , GenParticle { pbarcode = 8
                                                    , pidPDG = pdg2
                                                    , px = -px', py = -py', pz = -pz', pE = sqrt (px'^2 + py'^2 + pz'^2), generatedMass = 0.0
                                                    , statusCode = 1
                                                    , polTheta = 0, polPhi = 0 
                                                    , vbarcode4ThisIncoming = 0
                                                    , flows = (0,[]) 
                                                    } 
                                      ]
                            }
