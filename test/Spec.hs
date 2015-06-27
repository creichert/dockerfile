
import Test.Hspec
import Data.Docker


main :: IO ()
main = hspec $ do

  describe "Data.Docker" $ do

    it "sanity checks test-suite" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "supports FROM instruction" $ do
      dockerfile (from "ubuntu:trusty") `shouldBe` "FROM ubuntu:trusty\n"

    it "supports MAINTAINER instruction" $ do
      dockerfile (maintainer "Christopher Reichert <creichert07@gmail.com>")
        `shouldBe` "MAINTAINER Christopher Reichert <creichert07@gmail.com>\n"

    {-
    it "supports RUN instruction" $ do
      undefined
      -- let dockerfile = runDocker (maintainer "Christopher Reichert <creichert07@gmail.com>")
      -- dockerfile `shouldBe` "MAINTAINER Christopher Reichert <creichert07@gmail.com>\n"

    it "supports ENV instruction" $ do
      undefined
      -- let dockerfile = runDocker (maintainer "Christopher Reichert <creichert07@gmail.com>")
      -- dockerfile `shouldBe` "MAINTAINER Christopher Reichert <creichert07@gmail.com>\n"

    it "supports CMD instruction" $ do
      undefined
      -- let dockerfile = runDocker (maintainer "Christopher Reichert <creichert07@gmail.com>")
      -- dockerfile `shouldBe` "MAINTAINER Christopher Reichert <creichert07@gmail.com>\n"

    it "supports EXPOSE instruction" $ do
      undefined
      -- let dockerfile = runDocker (maintainer "Christopher Reichert <creichert07@gmail.com>")
      -- dockerfile `shouldBe` "MAINTAINER Christopher Reichert <creichert07@gmail.com>\n"

    it "supports ADD instruction" $ do
      undefined
      -- let dockerfile = runDocker (maintainer "Christopher Reichert <creichert07@gmail.com>")
      -- dockerfile `shouldBe` "ADD Christopher Reichert <creichert07@gmail.com>\n"
    -}
