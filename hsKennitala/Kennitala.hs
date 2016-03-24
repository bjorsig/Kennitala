module Kennitala where
import Text.Read
import Control.Applicative
import Data.Traversable

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

data Tegund = Einstaklingur | Lögaðili deriving (Show, Eq)
tegund :: Kennitala -> Tegund
tegund kt = if dagur kt == daggildi kt then Einstaklingur else Lögaðili

instance Show Kennitala where
    show (Kennitala daggildi mánuður ár nr vartala öld) =
        (show =<< (\t -> [t `div` 10, t `mod` 10])  =<< [daggildi,mánuður,ár])  ++ "-" ++ (show =<< [nr,vartala,öld])

isRight (Left  _) = False
isRight (Right _) = True

gildKennitala :: String -> Bool
gildKennitala = isRight . kennitala

breytaVillu :: (a -> b) -> Either a r -> Either b r
breytaVillu f = either (Left . f) Right

x & f = f x

krefjast :: (a -> Bool) -> String -> a -> Either String a
krefjast skilyrði melding x = if skilyrði x
    then Right x
    else Left melding

tala :: String -> (Int -> Bool) -> Char -> Char -> String -> Either String Int
tala heiti skilyrði a b melding = (readEither [a,b]
    & breytaVillu (const $ heiti ++ " má einungis innihalda tölustafi")
    >>= krefjast skilyrði melding
    >>= krefjast (0<) (heiti ++ " verður að vera jákvæð"))
    & breytaVillu (++ ", ekki " ++ [a,b] ++ ".")

tölustaf :: String -> Char -> Either String Int
tölustaf heiti a = let gildi = fromEnum a - fromEnum '0'
    in if   gildi > 9
       then Left $ heiti ++ " á að vera tölustafur, ekki " ++ [a] ++ "."
       else Right gildi

kennitala :: String -> Either String Kennitala
kennitala [dagtugur,dagur,mántugur,mánuður,áratugur,ár,nr1,nr2,vartala,öld] =
    Kennitala <$>
    tala "Dagsetning" (\dagur -> dagur <= 71 && (dagur <= 31 || 41 <= dagur) ) dagtugur dagur
        "Daggildi má mest vera 31 fyrir einstaklinga en minnst 41 fyrir félög. Daggildi má vera margt að 71" <*>
    tala "Mánuður" (<= 12) mántugur mánuður "Mánuðirnir eru tólf" <*>
    tala "Ártal" (const True) áratugur ár "Ár má tákna með hvaða tveggja stafa tölu sem er" <*>
    tala "Númer" (>= 20) nr1 nr2 "Einstaklingar eru númeraðir frá 20" <*>
    tölustaf "Vartala" vartala <*>
    tölustaf "Öld" öld
    >>= \kt ->
    vartölu [dagtugur,dagur,mántugur,mánuður,áratugur,ár,nr1,nr2]
        >>= maybe (Left "Þessi kennitala getur ekki verið rétt. Hún á sér ekki vartölu.")
        (\útreiknuð ->
            if útreiknuð == vartala
                then Right kt
                else Left $ "Vartalan " ++ [vartala] ++ " stemmir ekki við útreiknaða vartölu " ++ show útreiknuð ++ ".")
kennitala (dagtug:dag:mántug:mánuð:áratug:ár':'-':rest) = kennitala (dagtug:dag:mántug:mánuð:áratug:ár':rest)
kennitala _ = Left $ "Kennitala þarf að vera 10 tölustafir, að frátöldu valfrjálsu bandstriki."

-- Flestar 8 tölustafa runur eiga sér vartölu.
-- Stundum kemst út reiknuð vartala ekki fyrir í einum tölustaf.
-- Þá skilar eftirfarandi fall Nothing.
-- Það þýðir að kennitala getur ekki byrjað á tilteknu 8 tölustafa rununni.
-- Annaðhvort var runan rangt slegin inn, eða Þjóðskrá verður að velja nýtt númer.
-- Ef út reiknuð vartala kemst fyrir, þá skilar fallið Just henni.
vartöluFastar = 3:2:[7,6..2]
vartölu :: String -> Either String (Maybe Char)
vartölu strengur = if length strengur /= 8
    then Left "Vartala reiknast ekki nema af fyrstu 8 stöfum kennitölu."
    else do 
            listi <- traverse (tölustaf "Tölustafur") strengur :: Either String [Int]
            let niðurstaða = 11 - ((sum $ zipWith (*) vartöluFastar listi) `mod` 11)
            return . fmap (toEnum . (fromEnum '0' +)) $
                if niðurstaða == 11 then  Just 0
                else if niðurstaða == 10 then Nothing
                else Just niðurstaða
