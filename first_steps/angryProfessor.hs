-- https://www.hackerrank.com/challenges/angry-professor
canceled k xs = if length (filter (<=0) xs) >= k then"NO" else "YES"