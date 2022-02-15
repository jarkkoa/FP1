commonSubstring :: String -> String -> String
commonSubstring [] _ = ""
commonSubstring _ [] = ""
commonSubstring s1 s2
        | a1 `notElem` s2  = commonSubstring (tail s1) s2
        | a1 == a2 = a1 : commonSubstring (tail s2) (tail s1)
        | a1 /= a2 = commonSubstring s1 (tail s2)
        | otherwise = ""
        where a1 = head s1
              a2 = head s2