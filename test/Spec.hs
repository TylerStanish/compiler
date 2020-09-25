import Test.Hspec

import Parser

main :: IO ()
main = hspec $ do
  describe "Parser.parse" $ do
    it "parses 'module' keyword correctly" $ do
      (run Parser.mod "module foo") `shouldBe` (Right $ Module "foo")

    it "discards comments" $ do
      (run Parser.comment "//hithere\n") `shouldBe` (Right ())
      (run Parser.comment "//hithere") `shouldBe` (Right ())
      (run Parser.comment "//hithere//world//") `shouldBe` (Right ())

    it "parses identifiers" $ do
      (run Parser.ident "hello   \n\n\n   ") `shouldBe` (Right "hello")
      (run Parser.ident "hello   // world \n// jfkdlsa \n   // hjkl\n   ") `shouldBe` (Right "hello")

    it "parses function declarations" $ do
      let input = "def bla(){}"
      (run Parser.function input) `shouldBe` (Right $ Function "bla" [] [])
      let input = "def     bla  (   )   {   }    "
      (run Parser.function input) `shouldBe` (Right $ Function "bla" [] [])
