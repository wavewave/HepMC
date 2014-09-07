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
    B.putStrLn $ toByteString (buildEvent testevent)

                         

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
