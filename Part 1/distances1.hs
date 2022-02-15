distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 s1 s2 = realToFrac(doNotAppear + appear) / realToFrac(length s1 + length s2)
        where
            doNotAppear = length (myNub [c1 | c1 <- s1, c1 `notElem` s2])
            appear = length (myNub [c1 | c1 <- s1, c1 `elem` s2])

distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 = realToFrac(count1 + count2) / realToFrac(length s1 + length s2)
        where
            count1 = length [n | n <- s1, n `notElem` ['0'..'9']]
            count2 = length [n | n <- s2, n `notElem` ['0'..'9']]

picks :: [x] -> [([x], x, [x])]
picks []       = []
picks (x : xs) = ([], x, xs) : [(x : bs, y, as) | (bs, y, as) <- picks xs]

myNub :: Eq x => [x] -> [x]
myNub xs = [x | (bs, x, as) <- picks xs, x `notElem` bs]