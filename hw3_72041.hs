import Data.List
import Data.Char

main :: IO()
main = do
        
        
        print (getMoviesLongerThan "Star Wars" db)
        print (getMoviesLongerThan "The Fellowship of the Ring" db) 
        print (getMaleActorsIn "Terms of Endearment" db )
        print (getMaleActorsIn "Star Wars" db)
        print (getFemaleActorsFrom 1983 db)
        print (getFemaleActorsFrom 2001 db)

        print $ degr t1 5
        print $ degr t1 6
        print $ degr t1 7
        print $ degr t1 18
        print $ degr t2 's'
        print $ degr t2 'k'
        print $ degr t2 '1' 



--zadacha 1

type Name = String
type Title = String
type Year = Int
type Gender = Char
type Length = Int

data Movie = Movie Title Year Length 
  deriving(Show)
data MovieStar = MovieStar Name Gender 
  deriving (Show)
data StarsIn = StarsIn Name Title 
  deriving (Show)

type MovieDB = ([Movie], [MovieStar], [StarsIn])

movies :: [Movie]
movies = [Movie "The Man Who Wasn't There" 2001 116,
          Movie "Logan's run" 1976 120,
          Movie "Star Wars" 1977 124,
          Movie "Empire Strikes Back" 1980 111,
          Movie "Star Trek" 1979 132,
          Movie "Star Trek: Nemesis" 2002 116,
          Movie "Terms of Endearment" 1983 132,
          Movie "The Usual Suspects" 1995 106,
          Movie "Gone With the Wind" 1938 238,
          Movie "The Fellowship of the Ring" 2001 178]

stars :: [MovieStar]
stars = [MovieStar "Jane Fonda" 'F',
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
starsIn =  [StarsIn "Kim Basinger" "Star Wars",
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
db = (movies, stars, starsIn)

titleMovie :: Movie -> String 
titleMovie (Movie title _ _) = title

yearMovie :: Movie -> Int
yearMovie (Movie _ year _) = year

lengthMovie :: Movie -> Int
lengthMovie (Movie _ _ length) = length

nameMovieStar :: MovieStar -> String
nameMovieStar (MovieStar name _ ) = name

genderMovieStar :: MovieStar -> Char
genderMovieStar (MovieStar _ gender) = gender

nameStarsIn :: StarsIn -> String
nameStarsIn (StarsIn name _) = name

filmStarsIn :: StarsIn -> String
filmStarsIn (StarsIn _ film) = film

moviesDB :: MovieDB -> [Movie]
moviesDB (movies, _ , _) = movies

starsDB :: MovieDB -> [MovieStar] 
starsDB (_ , stars, _) = stars

starsInDB :: MovieDB -> [StarsIn]
starsInDB (_ , _, starsIn) = starsIn



getMoviesLongerThan ::Title -> MovieDB -> [Title]
getMoviesLongerThan name db = [ titleMovie x | x <- moviesDB db, (getFilmLenght name movies) < lengthMovie x ]

getFilmLenght :: Name -> [Movie] -> Int
getFilmLenght name (x:xs)
        | (titleMovie x) == name = (lengthMovie x)
        | otherwise = getFilmLenght name xs


getMaleActorsIn :: Title -> MovieDB -> [Name]
getMaleActorsIn title db = [ nameStarsIn x | x <- starsInDB db, (title == (filmStarsIn x)) && (isMale (nameStarsIn x) stars)]

isMale :: Name -> [MovieStar] -> Bool
isMale name (x:xs)
            | ((nameMovieStar x) == name) && ((genderMovieStar x) == 'M') = True
            | ((nameMovieStar x) == name) && ((genderMovieStar x) == 'F') = False
            | otherwise = isMale name xs


getFemaleActorsFrom :: Year -> MovieDB -> [Name]
getFemaleActorsFrom year db = [nameStarsIn x| x <- starsInDB db, (chekYear (filmStarsIn x) year movies) && (isFemale (nameStarsIn x) stars)]

chekYear :: Title -> Year -> [Movie] -> Bool
chekYear title year (x:xs)
      |((titleMovie x) == title) && ((yearMovie x) /= year) = False
      |((titleMovie x) == title) && ((yearMovie x) == year) = True
      | otherwise = chekYear title year xs

isFemale :: Name -> [MovieStar] -> Bool
isFemale name (x:xs)
      | (name == (nameMovieStar x)) && ((genderMovieStar x) == 'F') = True
      | (name == (nameMovieStar x)) && ((genderMovieStar x) == 'M') = False
      | otherwise = isFemale name xs

--zadacha2

data NTree a = Nil | Node a [NTree a]
     deriving (Show)

t1 :: NTree Int
t1 = Node 8 [(Node 7 [(Node 4[Nil]),(Node 5[Nil])]), (Node 6 [(Node 10[Nil]), (Node 15[Nil]), (Node 13[Nil])]), (Node 18[Nil])]

t2 :: NTree Char
t2 = Node '1' [(Node 'f' [ (Node 'H' [Nil]), (Node 'a' [Nil])]), (Node 'm' [(Node 's' [Nil])]), (Node 'i' [(Node 'k' [(Node 'e' [Nil]),(Node 'I' [Nil]),(Node 'L' [Nil])])])]


degr :: (Eq a) => NTree a -> a -> Int
degr Nil _ = 0
degr (Node v xs) n = if v == n then getSize (Node v xs)-1 else (findV xs n)


findV :: (Eq a) => [NTree a] -> a -> Int
findV [] _ = 0
findV xs n = (findV (tail xs) n ) + (helper (head xs) n )
   where 
      helper Nil _ =0
      helper (Node v xs) n = if v == n then getSize (Node v xs)  else findV xs n


getSize :: (Eq a) => NTree a -> Int
getSize (Node v xs) = helper (head xs) xs
    where
      helper Nil _ = 1
      helper _  xs = 1 + length xs


