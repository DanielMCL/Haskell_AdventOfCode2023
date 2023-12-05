zero = fromEnum '0'

main = do input <- readFile "day4input.txt"
          print $ sum . map points $ lines input

points string = ([0] ++ iterate (*2) 1) !! n
                 where n = (uncurry numWon) $ listWinners str where str = advancePastColon string

listWinners :: [Char] -> ([Char], [Int])                 
listWinners ('|':str) = (str, [])      
listWinners (' ':str) = listWinners str
listWinners str = (outStr, lw ++ [num]) where (num,(outStr, lw)) = (num, listWinners (advance str (numDigits + 1))) where (num, numDigits) = getNum str

numWon :: [Char] -> [Int] -> Int
numWon [] _ = 0
numWon (' ':str) lw = numWon str lw
numWon str lw = won + numWon (advance str (numDigits + 1)) lw where ((num, numDigits), won) = (getNum str, if (elem num lw) then 1 else 0)

advance :: [Char] -> Int -> [Char]
advance str 0 = str
advance [] _ = []
advance (c:str) x = advance str (x-1)

getNum :: [Char] -> (Int, Int) --(num, numDigits)                               
getNum [] = (0, 0)
getNum (x:xs) = if(n>=0 && n<10) then (prevNum + n*(10^prevNumDigits), prevNumDigits + 1) else (0, 0)
                    where (n, (prevNum,prevNumDigits)) = (fromEnum x - zero, getNum xs)

advancePastColon (':':str) = str
advancePastColon (_:str) = advancePastColon str