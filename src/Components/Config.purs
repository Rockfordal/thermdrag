module Components.Config (wsUrl, restUrl) where

import Prelude ((<>))


wsUrl :: String
wsUrl = "ws://" <> cidr

restUrl :: String
restUrl  = "http://" <> cidr


cidr :: String
cidr = hostname <> ":" <> port

hostname :: String
hostname = local

port :: String
port = "8081"

local :: String
local = "localhost"

fire :: String
fire = "46.162.127.62"