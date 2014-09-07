{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Parser.HepMC.Builder where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import           Control.Category ((>>>))
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Text as T

import HEP.Parser.HepMC.Type 

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
