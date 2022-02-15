charsDivisibleBy :: Int -> [Char]
charsDivisibleBy 0 = []
charsDivisibleBy n = [snd chr | chr <- zip [1..] ['a'..'z'], fst chr `mod` n == 0]

charsProductOf :: [Int] -> [Char]
charsProductOf [] = []
charsProductOf ints = [snd chr | chr <- zip [1..] ['a'..'z'], fst chr `elem` products ints]

products :: [Int] -> [Int]
products ints = [x*y | x <- ints, y <- ints, x /= y]
