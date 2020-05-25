{-# LANGUAGE OverloadedStrings #-}

import qualified View.Core as Vc
import Test.Hspec                ( Spec (..)
                                       , hspec
                                       , it
                                       , describe
                                       , shouldBe )

main :: IO ()
main = hspec $ do
    describe "showPico" $ do
        showPicoSpec

showPicoSpec :: Spec
showPicoSpec = do
    let go x = Vc.showPicoSec . truncate $ x * 10**12
    it "Works when x < 0" $ do
        go (-1)      `shouldBe` "<0 s"
        go (-0.01)   `shouldBe` "<0 s"
    it "Works when x < 1 ms" $ do
        go (-0.0000) `shouldBe` "<1 ms"
        go 0.0000    `shouldBe` "<1 ms"
        go 0.0001    `shouldBe` "<1 ms"
        go 0.0005    `shouldBe` "<1 ms"
    it "Works when 1 ms < x < 1000 ms" $ do
        go 0.0010    `shouldBe` "0.001 s"
        go 0.0100    `shouldBe` "0.010 s"
        go 0.1000    `shouldBe` "0.100 s"
        go 0.1111    `shouldBe` "0.111 s"
        go 0.0111    `shouldBe` "0.011 s"
        go 0.1011    `shouldBe` "0.101 s"
        go 0.1101    `shouldBe` "0.110 s"
        go 0.9999    `shouldBe` "0.999 s"
    it "Works when 1 s < x < 1000 s" $ do
        go 1         `shouldBe` "1.000 s"
        go 1.0010    `shouldBe` "1.000 s"
        go 1.0100    `shouldBe` "1.010 s"
        go 1.1000    `shouldBe` "1.100 s"
        go 1.1111    `shouldBe` "1.111 s"
        go 1.0111    `shouldBe` "1.011 s"
        go 1.1011    `shouldBe` "1.101 s"
        go 1.1101    `shouldBe` "1.110 s"
        go 1.9999    `shouldBe` "1.999 s"
        go 10        `shouldBe` "10.00 s"
        go 10.0010   `shouldBe` "10.00 s"
        go 10.0100   `shouldBe` "10.01 s"
        go 10.1000   `shouldBe` "10.10 s"
        go 10.1111   `shouldBe` "10.11 s"
        go 10.0111   `shouldBe` "10.01 s"
        go 10.1011   `shouldBe` "10.10 s"
        go 10.1101   `shouldBe` "10.11 s"
        go 10.9999   `shouldBe` "10.99 s"
        go 19.9999   `shouldBe` "19.99 s"
        go 100       `shouldBe` "100.0 s"
        go 100.0010  `shouldBe` "100.0 s"
        go 100.0100  `shouldBe` "100.0 s"
        go 100.1000  `shouldBe` "100.1 s"
        go 100.1111  `shouldBe` "100.1 s"
        go 100.0111  `shouldBe` "100.0 s"
        go 100.1011  `shouldBe` "100.1 s"
        go 100.1101  `shouldBe` "100.1 s"
        go 100.9999  `shouldBe` "100.9 s"
        go 190.9999  `shouldBe` "190.9 s"
        go 990.9999  `shouldBe` "990.9 s"
        go 909.9999  `shouldBe` "909.9 s"
        go 999.9999  `shouldBe` "999.9 s"
    it "Works when x >= 1000 s" $ do
        go 1000      `shouldBe` ">1 ks"
        go 1000.1    `shouldBe` ">1 ks"
