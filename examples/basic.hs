
{-# LANGUAGE OverloadedStrings #-}

import Data.Docker

main :: IO ()
main = do
  let df = dockerfile $ do
             from "debian:stable"
             maintainer "Christopher Reichert <creichert07@gmail.com>"
             run "apt-get -y update"
             run "apt-get -y upgrade"
             cmd [ "echo", "hello world"]
  putStrLn df
