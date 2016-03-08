module Kennitala where
import Text.Read
import Control.Applicative

data Kennitala = Kennitala {
                   daggildi :: Int,
                   mánuður :: Int,
                   ár :: Int,
                   nr :: Int,
                   vartala :: Int,
                   öld :: Int
                 }

dagur :: Kennitala -> Int
dagur (Kennitala daggildi _ _ _ _ _) =
    if daggildi > 31
        then daggildi - 40
    else
        daggildi

instance Show Kennitala where
    show (Kennitala daggildi mánuður ár nr vartala öld) =
        (show =<< [daggildi,mánuður,ár])  ++ "-" ++ (show =<< [nr,vartala,öld])

tala :: String -> Char -> Char -> Int
tala heiti a b = either (error $ heiti ++ " má einungis innihalda tölustafi.") id $ readEither [a,b]

tölustaf :: String -> Char -> Int
tölustaf heiti a = let t = fromEnum a - fromEnum '0'
    in if   t > 9
       then error (heiti ++ " á að vera tölustafur.")
       else t

vona :: (Monad m) => Bool -> String -> Int -> m Kennitala -> m Kennitala
vona condition melding gildi kt = if not condition then fail (melding ++ "—Ekki " ++ show gildi ++ ".") else kt

kennitala :: (Monad m) => String -> m Kennitala
kennitala [dagtugar,dags,mántugar,mánuðar,áratugar,árs,nr1,nr2,vartölu,aldar] =
    let dagur   = tala "Dagsetning" dagtugar dags
        mánuður = tala "Mánuður" mántugar mánuðar
        ár      = tala "Ártal" áratugar árs
        nr      = tala "Númer" nr1 nr2
        vartala = tölustaf "Vartala" vartölu
        öld     = tölustaf "Öld" aldar
    in
        vona (dagur <= 71) "Daggildi má mest vera 71." dagur $
        vona (dagur <= 31 || 41 <= dagur) "Daggildi má mest vera 31 fyrir einstaklinga en minnst 41 fyrir félög." dagur $
        vona  (mánuður <= 12) "Mánuðirnir eru ekki nema tólf." mánuður$
        vona  (nr >= 20)     "Einstaklingar eru númeraðir frá 20." nr $
        return $ Kennitala dagur mánuður ár nr vartala öld
kennitala (dagtug:dag:mántug:mánuð:áratug:ár':'-':rest) = kennitala (dagtug:dag:mántug:mánuð:áratug:ár':rest)
kennitala _ = fail "Kennitala þarf að vera 10 tölustafir, að frátöldu valfrjálsu bandstriki."

-- Flestar 8 tölustafa runur eiga sér vartölu.
-- Stundum kemst út reiknuð vartala ekki fyrir í einum tölustaf.
-- Þá skilar eftirfarandi fall Nothing.
-- Það þýðir að kennitala getur ekki byrjað á tilteknu 8 tölustafa rununni.
-- Annaðhvort var runan rangt slegin inn, eða Þjóðskrá verður að velja nýtt númer.
-- Ef út reiknuð vartala kemst fyrir, þá skilar fallið Just henni.
vartöluFastar = 3:2:[7,6..2]
vartölu :: [Int] -> Maybe Int
vartölu listi = if length listi /= 8
    then error "Vartala reiknast ekki nema af fyrstu 8 stöfum kennitölu."
    else let niðurstaða = 11 - ((sum $ zipWith (*) vartöluFastar listi) `mod` 11)
    in
        if niðurstaða == 11 then Just 0
        else if niðurstaða == 10 then Nothing
        else Just niðurstaða


main = (print =<< kennitala "211295-2019")
     >> putStr "Vartala: "
     >> maybe (error "Vartala kemst ekki fyrir í einum tölustaf") print (vartölu [2,1,1,2,9,5,2,0])
