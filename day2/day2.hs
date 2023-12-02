import Data.Char(digitToInt)

main = do games <- getGamesFile 
          print $ sum $ map idOrZero games  

getGamesFile::IO [[Char]]
getGamesFile = do file <- readFile "day2input.txt"
                  return $ lines file

idOrZero::[Char]->Int                         
idOrZero ('G':'a':'m':'e':' ':x:':':xs) = let (red, green, blue) = getMaximus xs in 
                                    if (red <= 12 && green <= 13 && blue <= 14) then
                                       digitToInt x else 0
idOrZero ('G':'a':'m':'e':' ':x:x2:':':xs) = let (red, green, blue) = getMaximus xs in 
                                    if (red <= 12 && green <= 13 && blue <= 14) then
                                       digitsToInt2 x x2 else 0
idOrZero ('G':'a':'m':'e':' ':x:x2:x3:':':xs) = let (red, green, blue) = getMaximus xs in 
                                    if (red <= 12 && green <= 13 && blue <= 14) then
                                       digitsToInt3 x x2 x3 else 0
                                       
getMaximus:: [Char]->(Int,Int,Int)
getMaximus [] = (0, 0, 0)
getMaximus (' ':xs) = getMaximus xs
getMaximus (';':xs) = getMaximus xs
getMaximus (',':xs) = getMaximus xs
getMaximus (x:' ':'r':'e':'d':xs) = let ((red, green, blue), r) = (getMaximus xs, digitToInt x)
                                           in if red >= r then (red, green, blue)
                                               else (r, green, blue)
getMaximus (x:x2:' ':'r':'e':'d':xs) = let ((red, green, blue), r) = (getMaximus xs, digitsToInt2 x x2)
                                           in if red >= r then (red, green, blue)
                                               else (r, green, blue)
                                       
getMaximus (x:' ':'g':'r':'e':'e':'n':xs) = let ((red, green, blue), g) = (getMaximus xs, digitToInt x) 
                                                   in if green >= g then (red, green, blue)
                                                       else (red, g, blue)
getMaximus (x:x2:' ':'g':'r':'e':'e':'n':xs) = let ((red, green, blue), g) = (getMaximus xs, digitsToInt2 x x2) 
                                                   in if green >= g then (red, green, blue)
                                                       else (red, g, blue)

getMaximus (x:' ':'b':'l':'u':'e':xs) = let ((red, green, blue), b) = (getMaximus xs, digitToInt x)
                                               in if blue >= b then (red, green, blue)
                                                   else (red, green, b)
getMaximus (x:x2:' ':'b':'l':'u':'e':xs) = let ((red, green, blue), b) = (getMaximus xs, digitsToInt2 x x2)
                                               in if blue >= b then (red, green, blue)
                                                   else (red, green, b)

digitsToInt2 x x2 = digitToInt x2 + 10 * (digitToInt x)
digitsToInt3 x x2 x3 = digitToInt x3 + 10 * (digitToInt x2) + 100 * (digitToInt x)