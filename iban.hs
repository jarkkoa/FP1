-- Checks if a Finnish IBAN is valid
validate :: String -> Bool
validate iban = if length iban /= 18 then False
                    else if read (convert (rearrange iban)) `mod` 97 == 1
                        then True
                        else False


-- Reverse to insert first 4 digits to the end, then reverse again
rearrange :: String -> String
rearrange (a:b:c:d:ds) = reverse (d:c:b:a: reverse ds)


-- Converts the IBAN to an integer representation
convert :: String -> String
convert str = if tail str == [] 
                then charToInt (head str) 
                else charToInt (head str) ++ convert (tail str)


-- Convert character to integer
charToInt :: Char -> String
charToInt character
        | character `elem` ['0'..'9'] = [character]
        | otherwise = show (head [int | (char, int) <-zip ['A'..'Z'] [10..35], char == character])
