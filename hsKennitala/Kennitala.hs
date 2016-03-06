module Kennitala where
import Text.Read
import Control.Applicative

tala :: String -> Char -> Char -> Int
tala heiti a b = either (error $ heiti ++ " má einungis innihalda tölustafi.") id $ readEither [a,b]

tölustaf :: String -> Char -> Int
tölustaf heiti a = let t = fromEnum a - fromEnum '0'
    in if   t > 9
       then error (heiti ++ " á að vera tölustafur.")
       else t

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

kennitala :: (Monad m) => String -> m Kennitala
kennitala [dagtugar,dags,mántugar,mánuðar,áratugar,árs,nr1,nr2,vartölu,aldar] =
    let dagur   = tala "Dagsetning" dagtugar dags
        mánuður = tala "Mánuður" mántugar mánuðar
        ár      = tala "Ártal" áratugar árs
        nr      = tala "Númer" nr1 nr2
        vartala = tölustaf "Vartala" vartölu
        öld     = tölustaf "Öld" aldar
    in
        if dagur > 71 || (31 < dagur && dagur < 41) then fail "Mánaðardagur má í mesta lagi vera 71 (eða 31 fyrir einstaklinga)."  else
        if  mánuður > 12 then fail "Mánuðirnir eru ekki nema tólf."                                     else
        if       nr < 20 then fail "Einstaklingar eru númeraðir frá 20."                                else
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


main = (print =<< kennitala "211295-2019") >> print (vartölu [2,1,1,2,9,5,2,0]) 
