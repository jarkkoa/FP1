onlyDigits :: String -> Bool
onlyDigits str
        | str == [] = False
        | (head str) `elem` ['0'..'9'] && (tail str) == [] = True
        | (head str) `elem` ['0'..'9'] = onlyDigits (tail str)
        | otherwise = False
