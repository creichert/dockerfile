
## `dockerfile` - A simple Dockerfile DSL for Haskell

### Examples

- Write a simple Dockerfile

      {-# LANGUAGE OverloadedStrings #-}

      import Data.Docker

      main :: IO ()
      main = putStrLn $ do
          dockerfile $ do
              from "debian:stable"
              env "DEBIAN_FRONTEND" "noninteractive"
              run "apt-get -y update"
              run "apt-get -y upgrade"
              cmd ["echo", "hello world"]

- Write a Dockerfile directly to a file

      {-# LANGUAGE OverloadedStrings #-}

      import Data.Docker

      main :: IO ()
      main = dockerfileWrite "Dockerfile.example $ do
          from "debian:stable"
          run "apt-get -y update"
          run "apt-get -y upgrade"
          cmd ["echo", "hello world"]

- Multi-stage build (a cleaner alternative to https://github.com/fpco/haskell-scratch):

      {-# LANGUAGE OverloadedStrings #-}

      import Data.Docker

      main :: IO ()
      main = putStrLn $ do

          dockerfile $ do

              fromas "debian:stable" "base"
              env "DEBIAN_FRONTEND" "noninteractive"

              run "apt-get -y update"
              run "apt-get -y install netbase"

              let prod_img_files = [
                      "/lib/x86_64-linux-gnu/libc.so.6"
                    , "/etc/protocols"
                    ]

              from "scratch"

              forM_ prod_img_files $ \fp -> do
                  copyfrom "base" fp fp

              entrypoint ["bash"]
