
import Data.Char(digitToInt)

main = 
 do x <- read_file
    print x

read_file :: IO Int
read_file = 
 do file <- readFile "day1input.txt"
    let file_lines = lines file
    return (sum (map sum_line file_lines))

sum_line:: [Char] -> Int
sum_line xs = if(not(second)) then y * 10 + y else y where (_, y, _, second) = sum_line2 xs

cero = fromEnum '0'

sum_line2:: [Char] -> (Bool, Int, Int, Bool)
sum_line2 [] = (True, 0, 0, False)
sum_line2 (x:xs) = 
 let (amIfirst, provisional_sol, first_value, thereIsSecond) = sum_line2 xs in
  let y = fromEnum x - cero in
   if(0 <= y && y < 10 && amIfirst) then (False, y, y, False)
     else if(0 <= y && y < 10) then (False, y * 10 + first_value, first_value, True)
      else (amIfirst, provisional_sol, first_value, thereIsSecond)

                             