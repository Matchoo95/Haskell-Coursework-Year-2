import Data.List
import System.IO
import Control.Exception

--
-- MATHFUN
-- UP769535
--

--
-- Types
--
type Title = String
type Director = String
type Year = Int
type Fans = [String]
type Film = (Title, Director, Year, Fans)

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
  --
  --
  --  Your functional code goes here
  --
  --

--1 add a new film to the database
addFilm :: Title -> Director -> Year -> [Film] -> [Film]
addFilm title director year film = (title, director, year, []) : film

--2 give all films in the database
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((title, director, year, fans):xs) = title ++ " by " ++ director ++
                                                   ", released " ++  (show year)
                                                   ++ ".\n" ++ filmsAsString xs

--3 give all the films that were released after a particular year (not including the given year)
displayFilmsAfterYear :: Year -> [Film] -> String
displayFilmsAfterYear year database = filmsAsString(filter(checkFilm year) database)

checkFilm :: Year -> Film -> Bool
checkFilm y (title, director, year, fans)
          | y < year = True
          | otherwise = False

--4 give all films that a particular user is a fan of
filmsByFan :: String -> [Film] -> String
filmsByFan fanName database = filmsAsStringFans(filter(isFan fanName) database)

isFan :: String -> Film -> Bool
isFan fanName (title, director, year, fans)
          | elem fanName fans = True
          | otherwise = False

--5 give all the fans of a particular film
{--
fansOfFilm :: [Film] -> Fans -> String
fansOfFilm filmName fans = fansAsString (filter(isFilm filmName) fans)

isFilm :: Title -> Film -> Bool
isFilm filmName (title, director, year, fans)
          | filmName == title = True
          | otherwise = False

fansAsString :: Fans -> String
fansAsString [] = ""
fansAsString  (x:xs) = (show x) ++ fansAsString ++ xs
                                                    --}
filmsAsStringFans :: [Film] -> String
filmsAsStringFans [] = ""
filmsAsStringFans  ((title, director, year, fans):xs) = title ++ " by " ++ director ++
                                                    ", released " ++  (show year)
                                                    ++ ".\nThe fans for this film are: "
                                                    ++ (show fans) ++ ".\n\n" ++ filmsAsStringFans xs


--6 allow a user to say they are a fan of a particular film
addFan :: String -> Title -> [Film] -> [Film]
addFan fan filmTitle [] = []
addFan fan filmTitle ((title, director, year, fans) : xs)
          | (filmTitle == title) && not(isFan fan (title, director, year, fans))
              = (title, director, year, fan : fans) : addFan fan filmTitle xs
          | otherwise = (title, director, year, fans) : addFan fan filmTitle xs

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2017 film "Alien: Covenant"
--                   by "Ridley Scott" to testDatabase
demo 1 = putStrLn(filmsAsString(addFilm "Alien: Covenant" "Ridley Scott" 2017 testDatabase))
--demo 2  = putStrLn (filmsAsString testDatabase)
demo 2 = putStrLn(filmsAsString testDatabase)
--demo 3  = putStrLn all films released after 2008
demo 3 = putStrLn(displayFilmsAfterYear 2008 testDatabase)
--demo 4  = putStrLn all films that "Liz" is a fan of
demo 4 = putStrLn(filmsByFan "Liz" testDatabase)
--demo 5  = putStrLn all fans of "Jaws"
--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
demo 6 = putStrLn(filmsAsStringFans(addFan "Liz" "The Fly" testDatabase))
demo 66 = putStrLn(filmsAsStringFans(addFan "Liz" "Avatar" testDatabase))
--demo 7 =  putStrLn all fans of films directed by "James Cameron"
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of

--
--
-- Your user interface code goes here
--
--
