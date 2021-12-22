credits :: (Char, Int) -> (Char, Int) -> Int
credits ('s',14) card2 = 14
credits card1 ('s',14) = 14
credits (s1, n1) (s2, n2) = if (succ n1 == n2) || (succ n2 == n1) && s1 == s2
                        then 8
                        else if n1 == n2
                            then 6
                            else if succ n1 == n2
                                then 4
                                else if s1 == s2
                                    then 2
                                    else 0