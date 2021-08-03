main :: IO()
main = do

        
        print (getFeaturedStars "MGM" 1995 db)
        print (getFeaturedStars "USA Entertainm." 2001 db)
        
        print (getPresident "Paramount" db)
        print (getPresident "Fox" db )
        print (getPresident "USA Entertainm." db)
        
        print (getHigherProductions "Calvin Coolidge" db)
        print (getHigherProductions "Stephen Spielberg" db)
        print (getHigherProductions "George Lucas" db )
        
        print $ toBinaryIndexed t1
        print $ toBinaryIndexed t2

--task 01

type Name = String
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int
type ProducerID = Int
type Networth = Integer

data Movie = Movie Title Year Length Name ProducerID
    deriving Show
data MovieStar = MovieStar Name Gender
    deriving Show
data StarsIn = StarsIn Name Title
    deriving Show
data Studio = Studio Name Int
    deriving Show
data MovieExec = MovieExec Name ProducerID Networth
    deriving Show

type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])


studios :: [Studio]
studios = [ Studio "Disney" 199,
            Studio "USA Entertainm." 222,
            Studio "Fox" 333,
            Studio "Paramount" 123,
            Studio "MGM" 555]

movieExecs :: [MovieExec]
movieExecs = [  MovieExec "George Lucas" 555 200000000,
                MovieExec "Ted Turner" 333 125000000,
                MovieExec "Stephen Spielberg" 222 100000000,
                MovieExec "Merv Griffin" 199 112000000,
                MovieExec "Calvin Coolidge" 123 20000000]

movies :: [Movie]
movies = [  Movie "Pretty Woman" 1990 119 "Disney" 199,
            Movie "The Man Who Wasn't There" 2001 116 "USA Entertainm." 555,
            Movie "Logan's run" 1976 120 "Fox" 333,
            Movie "Star Wars" 1977 124 "Fox" 555,
            Movie "Empire Strikes Back" 1980 111 "Fox" 555,
            Movie "Star Trek" 1979 132 "Paramount" 222,
            Movie "Star Trek: Nemesis" 2002 116 "Paramount" 123,
            Movie "Terms of Endearment" 1983 132 "MGM" 123,
            Movie "The Usual Suspects" 1995 106 "MGM" 199,
            Movie "Gone With the Wind" 1938 238 "MGM" 123,
            Movie "The Fellowship of the Ring" 2001 178 "USA Entertainm." 222]

stars :: [MovieStar]
stars = [   MovieStar "Jane Fonda" 'F',
            MovieStar "Alec Baldwin" 'M',
            MovieStar "Kim Basinger" 'F',
            MovieStar "Harrison Ford" 'M',
            MovieStar "Debra Winger" 'F',
            MovieStar "Jack Nicholson" 'M',
            MovieStar "Sandra Bullock" 'F',
            MovieStar "Orlando Bloom" 'M',
            MovieStar "Cate Blanchett" 'F',
            MovieStar "Liv Tyler" 'F',
            MovieStar "Billy Bob Thornton" 'M',
            MovieStar "Scarlett Johansson" 'F']

starsIn :: [StarsIn]
starsIn = [ StarsIn "Kim Basinger" "Star Wars",
            StarsIn "Alec Baldwin" "Star Wars",
            StarsIn "Harrison Ford" "Star Wars",
            StarsIn "Harrison Ford" "Empire Strikes Back",
            StarsIn "Jack Nicholson" "The Usual Suspects",
            StarsIn "Jane Fonda" "Terms of Endearment",
            StarsIn "Jack Nicholson" "Terms of Endearment",
            StarsIn "Sandra Bulloc" "The Usual Suspects",
            StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",
            StarsIn "Scarlett Johansson" "The Man Who Wasn't There",
            StarsIn "Orlando Bloom" "The Fellowship of the Ring",
            StarsIn "Cate Blanchett" "The Fellowship of the Ring",
            StarsIn "Liv Tyler" "The Fellowship of the Ring"]

db :: MovieDB
db = (movies, stars, starsIn, studios, movieExecs)



movieExecsDB :: MovieDB -> [MovieExec]
movieExecsDB ( _ , _ , _ , _ , movieexecs ) = movieexecs

studiosDB :: MovieDB -> [Studio]
studiosDB (_ , _ , _ , studios , _) = studios

getYearM :: Movie -> Int
getYearM (Movie _ year _ _ _ ) = year

titleMovie :: Movie -> String
titleMovie (Movie title _ _ _ _) = title

nameMovie :: Movie -> String
nameMovie (Movie _ _ _ name _) = name

numMovie :: Movie -> Int
numMovie (Movie _ _ _ _ num) = num

nameOfStar :: StarsIn -> String
nameOfStar (StarsIn name _) = name

studioName :: Studio -> String
studioName (Studio name _) = name

studioNum :: Studio -> Int
studioNum (Studio _ num) = num 

movieDB :: MovieDB -> [Movie]
movieDB (movies, _ , _ , _ , _) = movies

starsInDB :: MovieDB -> [StarsIn]
starsInDB (_ , _ , starsIn, _ , _) = starsIn

nameOfStarIn :: StarsIn -> String
nameOfStarIn (StarsIn name _) = name

titleOfStarIn :: StarsIn -> String
titleOfStarIn (StarsIn _ title) = title

movieExecsNum :: MovieExec -> Int
movieExecsNum (MovieExec _  num _) = num 

movieExecName :: MovieExec -> String
movieExecName (MovieExec name _ _) = name

movieNetworth :: MovieExec -> Integer
movieNetworth (MovieExec _ _ netw) = netw

getFeaturedStars :: Name -> Int -> MovieDB -> [Name]
getFeaturedStars name year db = [ nameOfStar ns | xs <- movieDB db, ns <-starsInDB db,(getYearM xs == year) && (titleOfStarIn ns == titleMovie xs) && (name == nameMovie xs)]

getPresident :: Name -> MovieDB -> Name
getPresident name db = helper name (studiosDB db) (movieExecsDB db)
        where
            helper name (x:xs) (n:ns)
                | null xs = ""
                | (studioName x == name) = (findName (studioNum x) db) 
                | otherwise              = helper name xs ns
                
findName :: Int -> MovieDB -> Name
findName num db = helper num (movieExecsDB db)
    where
        helper num (x:xs)
          | (num ==  movieExecsNum x) = movieExecName x
          | otherwise                   = helper num xs

getHigherProductions :: Name -> MovieDB -> [Name]
getHigherProductions name db = [titleMovie n| n <- movieDB db, (getNetWrk name) < getNetWrk' (numMovie n)]

getNetWrk :: String -> Integer
getNetWrk name  = helper name (movieExecsDB db)
    where
        helper name list
          | null list = 0
          | name == (movieExecName (head list)) = (movieNetworth (head list))
          | otherwise = helper name (tail list)

getNetWrk' :: Int -> Integer
getNetWrk' num = helper num (movieExecsDB db)
     where
         helper num (x:xs)
          | (null xs) = 0
          | (movieExecsNum x == num) = (movieNetworth x)
          | otherwise                       = helper num xs



--task 02

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show)

t1 :: BTree Char
t1 = Node 'a' (Node 'b' Nil (Node 'd' Nil Nil)) (Node 'c' (Node 'f' (Node 'e' Nil Nil) Nil) Nil)

t2 :: BTree Int
t2 = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))


toBinaryIndexed :: (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed tree = helper tree (zip (inorder tree)[0..])
    where
        helper ::(Eq a)=> BTree a -> [(a,Int)] -> BTree (a, Int)
        helper Nil _ = Nil
        helper _ [] = Nil
        helper (Node v lt rt) ((x,i):xs) 
            | v == x = Node (x,i) (helper lt xs)(helper rt xs)
            | otherwise = helper (Node v lt rt)(xs++[(x,i)])

inorder:: BTree a -> [a]
inorder Nil = []
inorder (Node v lt rt) = (inorder lt) ++ [v] ++ (inorder rt)


