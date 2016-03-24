import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Kennitala

main :: IO ()
main = hspec $ do
    describe "fmap show . kennitala" $
        it "athugar hvort kennitala sé lögleg og bætir við bandstriki ef vantar." $ do
            (fmap show . kennitala) "211295-2019" `shouldBe` Right "211295-2019"
            (fmap show . kennitala) "2112952019"  `shouldBe` Right "211295-2019"
            (fmap show . kennitala) "0101012049"  `shouldBe` Right "010101-2049"
            (fmap show . kennitala) "1001012119"  `shouldBe` Right "100101-2119"
    describe "gildKennitala" $ do
        it "staðfestir að mánaðardagur sé mest 31." $ do
            gildKennitala "7201002079"  `shouldBe` False
            gildKennitala "7112102109"  `shouldBe` True
            gildKennitala "3201002089"  `shouldBe` False
        it "staðfestir að mánaðardagur sé minnst 1." $ do
            gildKennitala "0012002599"  `shouldBe` False
            gildKennitala "-012002579"  `shouldBe` False
            gildKennitala "-912002509"  `shouldBe` False
        it "staðfestir að mánuður sé mest 12." $ do
            gildKennitala "010122-009"  `shouldBe` False
        it "staðfestir að mánuður sé minnst 1." $ do
            gildKennitala "0100122019"  `shouldBe` False
        it "passar að kennitala innihaldi einungis tölustafi." $ do
            gildKennitala "21129b-2019" `shouldBe` False
            gildKennitala "bókstaflega" `shouldBe` False
            gildKennitala "211295-29-9" `shouldBe` False
            gildKennitala "211295-291-" `shouldBe` False
        it "leyfir bandstrik, en einungis á réttum stað." $ do
            gildKennitala "211295-2019" `shouldBe` True
            gildKennitala "2112-952919" `shouldBe` False
        it "samþykkir gilda kennitölu." $ do
            gildKennitala "0108982509"  `shouldBe` True
            gildKennitala "1201603389"  `shouldBe` True
            gildKennitala "1010132009"  `shouldBe` True
        it "hafnar kennitölu af rangri lengd." $ do
            gildKennitala "123456789"   `shouldBe` False
            gildKennitala "12345678911" `shouldBe` False
    describe "vartölu" $ do
        it "reiknar út vartölu (sem bókstaf) útfrá fyrstu 8 tölustöfunum (sem streng)." $ do
            vartölu "12016033" `shouldBe` (Right $ Just '8')
        it "Skilar Right Nothing ef vartalan kemst ekki fyrir í einum tölustaf (bókstaf)." $
            vartölu "10101309" `shouldBe` Right Nothing
    describe "fmap kennitöluHafi . kennitala" $
        it "ákvarðar hvort kennitala myndi tilheyra einstaklingi eða lögaðila." $ do
            (fmap kennitöluHafi . kennitala) "1709715079"  `shouldBe` Right Einstaklingur
            (fmap kennitöluHafi . kennitala) "211295-2019" `shouldBe` Right Einstaklingur
            (fmap kennitöluHafi . kennitala) "4403044350"  `shouldBe` Right Lögaðili
            (fmap kennitöluHafi . kennitala) "440304-4350" `shouldBe` Right Lögaðili
