main :: IO ()
main = do 

        print ("Zad 1 a")
        print (safePrimesCount 20 100)
        print (safePrimesCount 1  983 )
        print (safePrimesCount 167 1892)
        print (safePrimesCount 1678 20097)
        print ("Zad 1 b")
        print (specialSum 3 20)
        print (specialSum 5 31)
        print (specialSum 8 10)
        print (specialSum 10 128)
        print ("Zad 2")
        print (validate 1714)
        print (validate 12345 )
        print (validate 891)
        print (validate 123)
        print (validate 2121)
        print (validate 4736778291034)
        print (validate 4485756008412)
        print (validate 4485756008422)
        print (validate 4214154976719)


--zad 1 a) Safe prime

safePrimesCount :: Integer -> Integer -> Integer
safePrimesCount a b = findSafePrime a b 0
         where 
              findSafePrime a b counter
                    | a > b   = counter
                    | (isPrime a) && (isPrime((a - 1) `div` 2)) = findSafePrime (a + 1) b (counter + 1)
                    | otherwise = findSafePrime (a + 1) b counter

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime a
      | a < 1         = isPrime (a + 1)
      | otherwise     = helper 2
                 where 
                     helper curDiv 
                         | curDiv == a             = True
                         | (a `mod` curDiv) == 0   = False
                         | otherwise               = helper (curDiv + 1)
                    
-- zad1 b Special sum

specialSum :: Integer -> Integer -> Integer
specialSum k m = helper k m 2 0 0 
          where 
              helper k m i sum counter
                 | counter == k                   = sum
                 | (isPrime i) && (((2^i)-1)>m)   = helper k m (i + 1) (sum + ((2^i)-1)) (counter+1)
                 | otherwise                      = helper k m (i + 1) (sum) (counter)


--zad2 validate

validate :: Int -> Bool 
validate num = findMod(chekSum(createNewList(makeNumInList num)))

makeNumInList :: Int -> [Int]
makeNumInList num = helper [] num 
       where
           helper xs num
              | num <=9   = xs ++[num]
              | otherwise = helper (xs ++ [(num `mod` 10)]) (num `div` 10)

createNewList :: [Int] -> [Int]
createNewList xs = helper [] xs 0
       where 
           helper zs xs i
              | ((i `mod` 2) == 0) && (i < (length xs))   = helper ((xs !! i) : zs) xs (i + 1)
              | ((i `mod` 2) /= 0) && (i < (length xs))   = helper ((createNewNumber ((xs !! i)*2)): zs) xs (i + 1)
              | otherwise                                 = zs

createNewNumber :: Int -> Int
createNewNumber num = if num < 9 then num else  (num `mod` 10) + (num `div` 10)

chekSum :: [Int] -> Int
chekSum [] = 0
chekSum (x : xs) = x + chekSum xs

findMod :: Int -> Bool
findMod  num = ((num `mod` 10) == 0)
