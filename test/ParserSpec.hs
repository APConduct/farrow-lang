module ParserSpec where
import Test.Hspec
import AST
import Parser

spec :: Spec 
spec = describe "Parser" $ do
    it "parses 'a'" $ do
        parseTest (char 'a') "a" `shouldBe` 'a'