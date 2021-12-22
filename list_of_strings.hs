headOrLast :: [String] -> Char -> [String]
headOrLast strList char = [str | str <- strList, head str == char || last str == char]