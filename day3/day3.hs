import Data.Char(digitToInt)
zero = fromEnum '0'

main = do engine <- readEngine 
          print $ sumOfParts engine  

readEngine::IO [[Char]]
readEngine = do file <- readFile "day3input.txt"
                return $ lines file
              
sumOfParts :: [[Char]] -> Int
sumOfParts (x:engine) = sumOfParts2 $ dots++(x:engine)++dots 
                         where dots = (take (length x) $ repeat '.':[]) 

sumOfParts2 (x:y:[]) = 0
sumOfParts2 (x:y:z:engine) = sumOfPartsLine ("."++y++".") (zip ("."++x++".") ("."++z++"."))   + sumOfParts2 (y:z:engine)

sumOfPartsLine :: [Char] -> [(Char, Char)] -> Int
sumOfPartsLine (z:[]) eng = 0
sumOfPartsLine (z:x:xs) (e:eng) = let y = fromEnum x - zero in
                                    if (y >= 0 && y < 10) then let (n, numDigits) = getNum (x:xs) in
                                                                let (nextXs, nextEng) = nextArgs numDigits (z:x:xs) (e:eng) in
                                                                 if (adyacentToSymbol numDigits (z:x:xs) (e:eng))then n + sumOfPartsLine nextXs nextEng
                                                                  else sumOfPartsLine nextXs nextEng
                                    else sumOfPartsLine (x:xs) eng

getNum :: [Char] -> (Int, Int) --(num, numDigits)                               
getNum [] = (0, 0)
getNum (x:xs) = if(n>=0 && n<10) then (prevNum + n*(10^prevNumDigits), prevNumDigits + 1) else (0, 0)
                    where (n, (prevNum,prevNumDigits)) = (fromEnum x - zero, getNum xs)
                    
adyacentToSymbol :: Int -> [Char] -> [(Char, Char)] -> Bool
adyacentToSymbol _ [] _ = False
adyacentToSymbol (-2) xs eng = False
adyacentToSymbol n (x:xs) ((y,z):eng) = let x2 = fromEnum x - zero
                                            y2 = fromEnum y - zero
                                            z2 = fromEnum z - zero in
                                                if((x/='.'&&(x2<0||x2>=10))|| 
                                                    (y/='.'&&(y2<0||y2>=10))||
                                                     (z/='.'&&(z2<0||z2>=10))) then True 
                                                      else (adyacentToSymbol (n-1) xs eng)
                                                      
nextArgs 0 xs eng = (xs, eng)
nextArgs n (x:xs) ((y,z):eng) = nextArgs (n-1) xs eng