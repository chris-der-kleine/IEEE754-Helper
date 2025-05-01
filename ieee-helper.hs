import Data.List (findIndex)
import Data.Maybe
import Data.Char
import Text.Read
import System.Environment

type Vorkomma = [Bool]
type Nachkomma = [Bool]
type BinaryZahl = [Bool]
type Exponent = [Bool]
type Charakteristik = [Bool]
data IEEE = ISingle | IDouble

version = "v1.1"


bin2nachkomma :: Nachkomma -> Double
bin2nachkomma =
    foldl (\acc (i,cur) -> if cur then acc + 1/(2^i) else acc ) 0.0
    . (zip [1..])

bin2vorkomma :: Vorkomma -> Double
bin2vorkomma =
    foldl (\acc (i,cur) -> if cur then acc + 2^i else acc ) 0.0
    . (zip [0..])
    . reverse


vorkomma2bin :: Int -> Vorkomma
vorkomma2bin i = conv i []
    where
        conv :: Int -> [Bool] -> Vorkomma
        conv 0 l = l
        conv f l =
            let
                rest = f `mod` 2
                zahl = floor $ (fromIntegral f) / 2
                b = if rest == 0 then False else True
            in
            conv zahl $ b : l


-- Achtung: Unendliche Liste!
nachkomma2bin :: Double -> Nachkomma
nachkomma2bin 0 = repeat False
nachkomma2bin 0.5 = True : repeat False
nachkomma2bin f = 
    if df > 1 then 
        True : (nachkomma2bin $ df-1)
    else 
        False : (nachkomma2bin $ df)
    where 
        df = 2*f
                

normalisierterWert :: Vorkomma -> Nachkomma -> (BinaryZahl, Int)
normalisierterWert vorkomma nachkomma =
    if length vorkomma == 1 then
        (nachkomma, 0)
    else if length vorkomma > 1 then
        ((tail vorkomma) ++ nachkomma, length vorkomma - 1 )
    else
        let
            mayIndex = findIndex (id) nachkomma
        in
        case mayIndex of
            Just index ->
                (snd (splitAt (index+1) nachkomma), (-1) - index) 
            Nothing ->
                -- Das Ding ist null!
                ([], 0)

denormalisierterWert :: (Maybe BinaryZahl, Maybe Int) -> (Maybe Vorkomma, Maybe Nachkomma)
denormalisierterWert (mbin, mexponent) =
    case (mbin,mexponent) of
        (Just bin, Just exponent) ->
            let
                (vorkomma, nachkomma) = splitFillWith (exponent+1) False (True:bin)
            in
            if (exponent > 0) && (exponent+1 > length vorkomma) then
                (Just vorkomma, Just nachkomma)
            else
                (Just vorkomma, Just nachkomma)
        _ -> (Nothing, Nothing)

-- Reverse SequenceA:  fromMaybe [] $ map (pure :: Int -> Maybe Int) <$> Just [1,0,1]

-- -------------------------------------------------------------------------------------------------------------------------
-- IO
-- -------------------------------------------------------------------------------------------------------------------------

bin2dez :: String -> Double
bin2dez l = 
    (bin2vorkomma  $ map (=='1') vorkomma)
    +
    (bin2nachkomma $ map (=='1') nachkomma)
    where
        (vorkomma, nachkomma) =
            splitOn '.' l



dez2bin :: Int -> Double -> String
dez2bin p f =
    (map boolShow $ vorkomma2bin vorkomma)
    ++ "."
    ++ (map boolShow $ take p $ nachkomma2bin nachkomma)

    where 
        vorkomma = fromIntegral $ floor f
        nachkomma = f - (fromIntegral vorkomma)
        boolShow = (\ a -> if a then '1' else '0')



construct2IEEE :: IEEE -> Double -> String
construct2IEEE ieee f = printIEEE $
    case ieee of
        ISingle ->   buildIEEE 23 8 f
        IDouble ->   buildIEEE 52 11 f
    where
        printIEEE ( vrz, char, man ) =
            boolShow [vrz] ++ char ++ man



buildIEEE :: Int -> Int -> Double -> (Bool, String, String)
buildIEEE p n f =
    (vrz
    , boolShow $ boolFillBefore n $ vorkomma2bin character 
    , boolShow $ boolFillAfter p $ take p $ mantisse)
    where
        vrz = f < 0

        character =  2^(n-1) - 1 + exponent

        (mantisse, exponent) = 
            normalisierterWert vorkomma nachkomma

        vorkomma = 
            vorkomma2bin $ fromIntegral $ floor $ abs f

        nachkomma = 
            take p $ nachkomma2bin $ (abs f) - (fromIntegral $ floor $ abs f)

   
destructIEEE :: Maybe String -> Maybe Double
destructIEEE (Just str) =
    if length str == 32 then
        destruct 8 str
    else if length str == 64 then
        destruct 11 str
    else
        Nothing

    where
        destruct :: Int -> String -> Maybe Double
        destruct n input =
            let
            vrz = if head input == '1' then (-1) else 1
            (character, mantisse) = splitAt n $ tail input
            exponent = 
                (\i -> i - (2^(n-1) - 1))
                <$> fromIntegral . floor . bin2vorkomma
                <$> parseBool character 
            (vorkomma, nachkomma) = 
                denormalisierterWert (parseBool mantisse, exponent)
            in
            (*) <$> pure vrz 
            <*> ((+)
              <$> (bin2vorkomma <$> vorkomma) 
              <*> (bin2nachkomma <$> nachkomma))
destructIEEE Nothing = Nothing
             

        
-- -------------------------------------------------------------------------------------------------------------------------
-- Main Programm
-- -------------------------------------------------------------------------------------------------------------------------

printError :: String
printError = "Invalid Arguments\nTry ieee-helper --help to get help"

printWithDefault :: String -> (String -> String) -> [String] -> String
printWithDefault def _ [] = def
printWithDefault _ f (a:_) = f a

printToIEEE :: [String] -> String
printToIEEE (mode:value_in:_) =
    let
        fp = (readMaybe value_in) :: Maybe Double
    in
    fromMaybe printError $
        printPretty <$>
            case map toLower mode of
                "single" -> construct2IEEE ISingle <$> fp
                "s"      -> construct2IEEE ISingle <$> fp
                "double" -> construct2IEEE IDouble <$> fp
                "d"      -> construct2IEEE IDouble <$> fp
                _        -> Nothing

    where
        printPretty s = unlines
            [ "After Formatting for IEEE754 your number becomes:"
            , bin2Hex s
            , "or in Binary:"
            , s
            ]
printToIEEE _ = printError

printDecToBin :: [String] -> String
printDecToBin (p:num:_) = 
    fromMaybe printError $ printPretty <$> (dez2bin <$> readMaybe p <*> readMaybe num)
    where
        printPretty :: String -> String
        printPretty v = unlines
            [ "Your Decimal number \"" ++ (show $ (read num :: Double)) ++ "\" is the following in binary:"
            , v
            , "You could also just write " ++ bin2Hex v ++ " if that is easier."
            ]
printDecToBin _ = printError

printIEEEToFp :: [String] -> String
printIEEEToFp (value_in:_) =
    let
        (hexIdent, hVal) = splitAt 2 value_in
    in
    fromMaybe "That didn't work :(" $
        printPretty <$>
            if hexIdent == "0x" then
                showPretty <$> destructIEEE (hex2Bin value_in)
            else
                showPretty <$> destructIEEE (pure value_in)
    where
        printPretty s = unlines
            [ "After reading your IEEE754 input the value is:"
            , s
            ]
        showPretty :: Double -> String
        showPretty = show
printIEEEToFp _ = printError


mainInteract :: [String] -> String
mainInteract (mode:args) =
    case mode of
        "BinToHex"  -> printWithDefault
            printError bin2Hex args

        "HexToBin"  -> printWithDefault 
            printError (fromMaybe printError . hex2Bin) args
        "DecToBin"  -> printDecToBin args
        "BinToDec"  -> printWithDefault
            printError (("Your Number is: " ++) . show . bin2dez) args

        "DecToIEEE" -> printToIEEE args
        "IEEEToDec" -> printIEEEToFp args
        "-v"        -> version
        "--version" -> version
        "-h"        -> printHelp
        "--help"    -> printHelp
        _           -> printError
mainInteract _      =  printHelp

main :: IO ()
main = do
    args <- getArgs
    putStr $ mainInteract args


printHelp :: String
printHelp = unlines
    [ "IEEE Helper Tool" ,""
    , "Usage:"
    , "  ieee-helper BinToHex <binNumber>"
    , "  ieee-helper HexToBin <hexNumber>"
    , "  ieee-helper DecToBin <precision> <decNumber>"
    , "  ieee-helper BinToDec <binNumber>"
    , "  ieee-helper DecToIEEE [(Single|s)|(Double|d)] <decNumber>"
    , "  ieee-helper IEEEToDec <ieee754BinNumber>"
    , "  ieee-helper -h | --help"
    , "  ieee-helper -v | --version"
    , ""
    , "Options:"
    , "  -h --help\t\tShow this help screen"
    , "  -v --version\t\tShow current Version"
    , ""
    , "Command Descriptions:"
    , "  BinToHex <binNumber>\t\t\t\tConvert a binary number to a hexadecimal number."
    , "  HexToBin <hexNumber>\t\t\t\tConvert a hexadecimal number to a binary number."
    , "  DecToBin <precision> <decNumber>\t\tConvert a decimal number to a binary number."
    , "  \t\t\t\t\t\t  Precision is the number of bits after the point."
    , "  BinToDec <binNumber>\t\t\t\tConvert binary floating number to decimal number."
    , "  DecToIEEE [(Single|s)|(Double|d)] <decNumber>\tConvert a decimal number to a IEEE754 compliant number."
    , "  IEEEToDec <ieee754BinNumber>\t\t\tInput a IEEE754 binary number and get the decimal representation."
    , ""
    , "Argument Descriptions:"
    , "  binaryNumber\t\tBinary number. Don't seperate by spaces. E.g. 0100101101"
    , "  hexNumber\t\tHexadecimal number. E.g. 0x42DF"
    , "  precision\t\tHow many digits after the point should be calculated? E.g. 3"
    , "  decNumber\t\tFloating point decimal number. E.g. 3.14"
    , "  ieee754BinNumber\tIEEE754 compliant bin number. E.g. 01000000010010001111010111000010"
    , ""
    , "Examples:"
    , "  ieee-helper HexToBin 0x43D.54"
    , "  ieee-helper DecToBin 23 3.1416"
    , "  ieee-helper DecToIEEE Single 42.743"
    , "  ieee-helper IEEEToDec 0x42b844dd"
    , "  ieee-helper BinToHex 110110.1011001"
    ]


-- -------------------------------------------------------------------------------------------------------------------------
-- Helpers
-- -------------------------------------------------------------------------------------------------------------------------

splitOn :: Eq e => e -> [e] -> ([e], [e])
splitOn e l =
    case t of
        [] -> (h, [])
        (x:_) -> (h, tail t)
    where
        (h, t) =
            splitAt (fromMaybe 0 (findIndex (== e) l)) l

splitFillWith :: Int -> a -> [a] -> ([a], [a])
splitFillWith index def ls =
    if index > (length ls) then
        ( ls ++ replicate (index - (length ls)) def , [] )
    else if index < 0 then
        ( [], replicate (0-index) def ++ ls )
    else
        splitAt index ls

bin2Hex :: String -> String
bin2Hex str =
    
    if '.' `elem` str then
        let
            (vorkomma, nachkomma) = splitOn '.' str
        in
        "0x" ++     doConversion vorkomma 
        ++ "." ++   doConversionR nachkomma
    else if ',' `elem` str then
        let
            (vorkomma, nachkomma) = splitOn ',' str
        in
        "0x" ++     doConversion vorkomma 
        ++ "." ++   doConversionR nachkomma
    else
        "0x" ++     doConversion str
        
    where
        doConversion  = showHex . convert . reverse  . makeInt
        doConversionR = showHex . reverse . convertR . makeInt

        makeInt = map (\c -> if c == '1' then 1 else 0)

        convert [] = []
        convert (a:b:c:d:xs) = d*8 + c*4 + b*2 + a : convert xs
        convert (a:b:c:xs) = c*4 + b*2 + a : convert xs
        convert (a:b:xs) = b*2 +a : convert xs
        convert (a:xs) = a : convert xs

        convertR [] = []
        convertR (d:c:b:a:xs) = d*8 + c*4 + b*2 + a : convertR xs
        convertR (d:c:b:xs) = d*8 + c*4 + b*2 : convertR xs
        convertR (d:c:xs) = d*8 + c*4 : convertR xs
        convertR (d:xs) = d*8 : convertR xs

showHex :: [Int] -> String
showHex [] = ""
showHex (x:xs)
    | x <= 9 = showHex xs ++ show x
    | x == 10 = showHex xs ++ "A"
    | x == 11 = showHex xs ++ "B"
    | x == 12 = showHex xs ++ "C"
    | x == 13 = showHex xs ++ "D"
    | x == 14 = showHex xs ++ "E"
    | x == 15 = showHex xs ++ "F"
    | otherwise = "0"

hex2Bin :: String -> Maybe String
hex2Bin str = 
    concat <$> 
    (sequenceA . init . convert . snd . splitAt 2 . map toUpper) str
    where
        convert [] = [Nothing]
        convert (x:xs)
            | x == '0' = Just "0000" : convert xs
            | x == '1' = Just "0001" : convert xs
            | x == '2' = Just "0010" : convert xs
            | x == '3' = Just "0011" : convert xs
            | x == '4' = Just "0100" : convert xs
            | x == '5' = Just "0101" : convert xs
            | x == '6' = Just "0110" : convert xs
            | x == '7' = Just "0111" : convert xs
            | x == '8' = Just "1000" : convert xs
            | x == '9' = Just "1001" : convert xs
            | x == 'A' = Just "1010" : convert xs
            | x == 'B' = Just "1011" : convert xs
            | x == 'C' = Just "1100" : convert xs
            | x == 'D' = Just "1101" : convert xs
            | x == 'E' = Just "1110" : convert xs
            | x == 'F' = Just "1111" : convert xs
            | otherwise = [Nothing]



boolFillBefore :: Int -> [Bool] -> [Bool]
boolFillBefore n x = 
    replicate (n - length x) False ++ x

boolFillAfter :: Int -> [Bool] -> [Bool]
boolFillAfter n x = 
    x ++ replicate (n - length x) False

boolShow :: [Bool] -> String
boolShow =
    map (\ a -> if a then '1' else '0')

parseBool :: String ->  Maybe [Bool]
parseBool = sequenceA . map toBool
    where
        toBool c =
            if c == '1' then        Just True
            else if c == '0' then   Just False
            else                    Nothing