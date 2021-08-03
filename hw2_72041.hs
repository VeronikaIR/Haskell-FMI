
main :: IO ()
main = do

         print("Zad1")  
         print(countRats ")1)1)1)1 P") 
         print(countRats "P 1( 1( )1 1(") 
         print(countRats " P 1( 1( )1 1(")
         print(countRats ")1)1)1)1P)1)11(")
         print(countRats "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(")
         print("Zad2")
         print ((josephus [1,2,3,4,5,6,7]) 3)
         print ((josephus [1,2,3,4,5,6,7,8,9,10]) 1 )
         print ((josephus [1,2,3,4,5,6,7,8,9,10]) 2)
         print ((josephus "fpFMIsu") 4)
         print ((josephus [1,2,3,4,5,6,7]) (-1)) -- test when k is not natural

--zad 1 

countRats :: String -> Int
countRats str = helper str []
      where 
          helper str zs
            | head str == 'P' = length zs + length([x | x <-(tail str), x == ')'])
            | head str == '(' = helper (tail str) (head str : zs)
            | otherwise       = helper (tail str) zs

-- zad 2

josephus :: [a] -> (Int -> [a])
josephus (x:xs) k = f (x:xs) k
f :: [a] -> Int -> [a]
f (x:xs) k | k < 0 = error "k was not natural"
f (x:xs) k = helper [] (x:xs) k 1
        where 
            helper zs (x:xs) k counter
               | null xs        = reverse (x : zs)
               | counter /= k   = helper zs (xs++[x]) k (counter+1)
               | counter == k   = helper (x:zs) xs k 1