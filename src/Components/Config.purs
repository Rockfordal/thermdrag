module Components.Config where

import Prelude ((<>))

wsUrl :: String
wsUrl = "ws://" <> cidr

restUrl :: String
restUrl  = "http://" <> cidr

cidr :: String
cidr = hostname <> ":" <> port

hostname :: String
hostname = localhost

port :: String
port = "8081"

localhost :: String
localhost = "localhost"

fire :: String
fire = "46.162.127.62"