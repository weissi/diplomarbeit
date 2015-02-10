--------------------------------------------------------------------------------
--  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").         --
--                                                                            --
--  diplomarbeit is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  diplomarbeit is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.     --
--                                                                            --
--  Copyright 2012, Johannes Weiß                                             --
--------------------------------------------------------------------------------

-- | Functions for the network setup phase between the two parties Goliath and
-- David.
module Functionality.SetupPhase
    ( SetupDavidToGoliath(..), SetupGoliathToDavid(..)
    , sg2dParseConduit, sd2gParseConduit
    , sg2dSerializeConduit, sd2gSerializeConduit
    , clientSettingsFromSetupD2G, clientSettingsFromSetupG2D
    , setupG2DFromClientSettings, setupD2GFromClientSettings
    ) where

-- # STDLIB

-- # SITE PACKAGES
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit (Conduit, (=$=))
import Text.ProtocolBuffers.Basic (uFromString, uToString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN

-- # LOCAL
import Data.Conduit.ProtoBufConduit

-- # PROTOBUF
import qualified Data.ProtoBufs.Setup.SetupDavidToGoliath as Pb
import qualified Data.ProtoBufs.Setup.SetupGoliathToDavid as Pb

data SetupGoliathToDavid = SetupGoliathToDavid { sg2dTokenHost :: String
                                               , sg2dTokenPort :: Int
                                               } deriving Show

data SetupDavidToGoliath = SetupDavidToGoliath { sd2gHost :: String
                                               , sd2gPort :: Int
                                               } deriving Show

clientSettingsFromSetupD2G :: SetupDavidToGoliath -> CN.ClientSettings
clientSettingsFromSetupD2G (SetupDavidToGoliath host port) =
    CN.clientSettings port (BS8.pack host)

clientSettingsFromSetupG2D :: SetupGoliathToDavid -> CN.ClientSettings
clientSettingsFromSetupG2D (SetupGoliathToDavid tokenHost tokenPort) =
    CN.clientSettings tokenPort (BS8.pack tokenHost)

setupG2DFromClientSettings :: CN.ClientSettings -> SetupGoliathToDavid
setupG2DFromClientSettings cs =
    SetupGoliathToDavid ((BS8.unpack . CN.getHost) cs) (CN.getPort cs)

setupD2GFromClientSettings :: CN.ClientSettings -> SetupDavidToGoliath
setupD2GFromClientSettings cs =
    SetupDavidToGoliath ((BS8.unpack . CN.getHost) cs) (CN.getPort cs)

sg2dParseConduit :: MonadResource m => Conduit ByteString m SetupGoliathToDavid
sg2dParseConduit = pbufParse =$= CL.map sg2dParse

sd2gParseConduit :: MonadResource m => Conduit ByteString m SetupDavidToGoliath
sd2gParseConduit = pbufParse =$= CL.map sd2gParse

sg2dSerializeConduit :: MonadResource m
                     => Conduit SetupGoliathToDavid m ByteString
sg2dSerializeConduit = CL.map sg2dSerialize =$= pbufSerialize

sd2gSerializeConduit :: MonadResource m
                     => Conduit SetupDavidToGoliath m ByteString
sd2gSerializeConduit = CL.map sd2gSerialize =$= pbufSerialize

sg2dParse :: Pb.SetupGoliathToDavid -> SetupGoliathToDavid
sg2dParse (Pb.SetupGoliathToDavid encTokenHost encTokenPort) =
    SetupGoliathToDavid (uToString encTokenHost) (fromIntegral encTokenPort)

sd2gParse :: Pb.SetupDavidToGoliath -> SetupDavidToGoliath
sd2gParse (Pb.SetupDavidToGoliath encHost encPort) =
    SetupDavidToGoliath (uToString encHost) (fromIntegral encPort)

sg2dSerialize :: SetupGoliathToDavid -> Pb.SetupGoliathToDavid
sg2dSerialize (SetupGoliathToDavid tokenHost tokenPort) =
    Pb.SetupGoliathToDavid (uFromString tokenHost) (fromIntegral tokenPort)

sd2gSerialize :: SetupDavidToGoliath -> Pb.SetupDavidToGoliath
sd2gSerialize (SetupDavidToGoliath host port) =
    Pb.SetupDavidToGoliath (uFromString host) (fromIntegral port)
