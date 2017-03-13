import Data.List

type Title = String
type Director = String
type Year = Int
type Fans = [String]

type Film = (Title, Director, Year, Fans)
type Database = [Film] -- TODO check if i need this at the end and remove if not

testDatabase :: [Film]
testDatabase = [
  ("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"]),
  ("The Fly", "David Cronenberg", 1986, ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
  ("Body Of Lies", "Ridley Scott", 2008, ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
  ("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"]),
  ("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"]),
  ("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
  ("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"]),
  ("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"]),
  ("Prometheus", "Ridley Scott", 2012, ["Kevin", "Tim", "Emma", "Jo", "Liz"]),
  ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"]),
  ("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"]),
  ("Jaws", "Steven Spielberg", 1975, ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"]),
  ("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"]),
  ("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"]),
  ("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"]),
  ("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"]),
  ("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"]),
  ("True Lies", "James Cameron", 1994, ["Sam", "Dave"]),
  ("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"]),
  ("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"]),
  ("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"]),
  ("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"]),
  ("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"]),
  ("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"]),
  ("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"])
  ]


-- Functional code

--1
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm title director year film = (title, director, year, []) : film

--2
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString  ((title, director, year, fans):xs) = title ++ " by " ++ director ++
                                                    ", released " ++  (show year)
                                                    ++ ".\n" ++ filmsAsString xs

--3
displayFilmsAfterYear :: Year -> [Film] -> String
displayFilmsAfterYear year database = filmsAsString (filter(checkFilm year) database)

checkFilm :: Year -> Film -> Bool
checkFilm year (t, d, y, f)
          | year <= y = True
          | otherwise = False

--4
filmsByFan fanName = filter (checkByFan fanName)

checkByFan :: String -> Film -> Bool
checkByFan fanName (title, director, year, fans)
          | elem fanName fans = True
          | otherwise = False

--5