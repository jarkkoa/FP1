nextIsGreater :: [Int] -> [Int]
nextIsGreater ints = [a | (a,b) <- zip ints (tail ints), a < b]
