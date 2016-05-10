import Test.Hspec
import Kennitala

main :: IO ()
main = hspec $ do
    describe "fmap show . kennitala" $
        it "Athugar hvort kennitala sé lögleg og bætir við bandstriki ef vantar." $ do
            (fmap show . kennitala) "211295-2019" `shouldBe` Right "211295-2019"
            (fmap show . kennitala) "2112952019"  `shouldBe` Right "211295-2019"
            (fmap show . kennitala) "0101012049"  `shouldBe` Right "010101-2049"
            (fmap show . kennitala) "1001012119"  `shouldBe` Right "100101-2119"
            (fmap show . kennitala) "1110571849"  `shouldBe` Right "111057-1849"
            (fmap show . kennitala) "0209581979"  `shouldBe` Right "020958-1979"
            (fmap show . kennitala) "0102560039"  `shouldBe` Right "010256-0039"

    describe "gildKennitala" $ do
        it "Man að ap-jún-sept-nóv þrjátíu hver," $ do
            gildKennitala "0404042019" `shouldBe`  True
            gildKennitala "3104042099" `shouldBe`  False

            gildKennitala "1111112009" `shouldBe`  True
            gildKennitala "3111112059" `shouldBe`  False
        it "en einn til hinir taka sér." $ do
            gildKennitala "1010102019" `shouldBe`  True
            gildKennitala "3110102049" `shouldBe`  True
        it "Veit að hlaupársdagur er ekki nema á fjögurra ára fresti." $ do
            gildKennitala "290201-2099" `shouldBe` False
        it "Staðfestir að mánaðardagur sé mest 31." $ do
            gildKennitala "7201002079"  `shouldBe` False
            gildKennitala "7112102109"  `shouldBe` True
            gildKennitala "7010810569"  `shouldBe` True
            gildKennitala "3201002089"  `shouldBe` False
        it "Staðfestir að mánaðardagur sé minnst 1." $ do
            gildKennitala "0012002599"  `shouldBe` False
            gildKennitala "-012002579"  `shouldBe` False
            gildKennitala "-912002509"  `shouldBe` False
        it "Staðfestir að mánuður sé mest 12." $ do
            gildKennitala "010122-009"  `shouldBe` False
        it "Staðfestir að mánuður sé minnst 1." $ do
            gildKennitala "0100122019"  `shouldBe` False
        it "Passar að kennitala innihaldi einungis tölustafi." $ do
            gildKennitala "21129b-2019" `shouldBe` False
            gildKennitala "bókstaflega" `shouldBe` False
            gildKennitala "211295-29-9" `shouldBe` False
            gildKennitala "211295-291-" `shouldBe` False
        it "Leyfir bandstrik, en einungis á réttum stað." $ do
            gildKennitala "211295-2019" `shouldBe` True
            gildKennitala "2112-952919" `shouldBe` False
        it "Samþykkir venjulega kennitölu einstaklings." $ do
            gildKennitala "0108982509"  `shouldBe` True
            gildKennitala "1201603389"  `shouldBe` True
            gildKennitala "1010132009"  `shouldBe` True
        it "Hafnar kennitölu af rangri lengd." $ do
            gildKennitala "123456789"   `shouldBe` False
            gildKennitala "12345678911" `shouldBe` False
        it "Samþykkir kennitölu með lága raðtölu." $ do
            gildKennitala "1110571849"  `shouldBe` True
            gildKennitala "0209581979"  `shouldBe` True
            gildKennitala "0102560039"  `shouldBe` True

    describe "vartölu" $ do
        it "Reiknar út vartölu (sem bókstaf) útfrá fyrstu 8 tölustöfunum (sem streng)." $ do
            vartölu "12016033" `shouldBe` (Right $ Just '8')
        it "Skilar Right Nothing ef vartalan kemst ekki fyrir í einum tölustaf (bókstaf)." $
            vartölu "10101309" `shouldBe` Right Nothing
    describe "fmap kennitöluHafi . kennitala" $
        it "Ákvarðar hvort kennitala myndi tilheyra einstaklingi eða lögaðila." $ do
            (fmap kennitöluHafi . kennitala) "1709715079"  `shouldBe` Right Einstaklingur
            (fmap kennitöluHafi . kennitala) "211295-2019" `shouldBe` Right Einstaklingur
            (fmap kennitöluHafi . kennitala) "4403044350"  `shouldBe` Right Lögaðili
            (fmap kennitöluHafi . kennitala) "440304-4350" `shouldBe` Right Lögaðili
