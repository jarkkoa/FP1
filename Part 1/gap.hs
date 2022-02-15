gap :: (Char, Char) -> Int -> String -> Int
gap (_, _) _ [] = 0
gap (c1, c2) g s
            | length s <= g + 1 = 0 
            | c1 == a && c2 /= b = gap (c1, c2) g (tail s)
            | c1 == a && c2 == b = gap (c1, c2) g (tail s) + 1
            | otherwise = gap (c1, c2) g (tail s)
        where a = head s
              b = s !! (g + 1)