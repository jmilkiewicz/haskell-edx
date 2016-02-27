-- https://www.hackerrank.com/challenges/plus-minus
positiveF xs = fraction (>0) xs
negativeF xs = fraction (<0) xs
neutralF xs = fraction (==0) xs

fraction p xs = fromIntegral(length (filter p xs))  / fromIntegral ((length xs))