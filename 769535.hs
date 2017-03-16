import Data.List

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
data Film = Film Title Director Year Fans

testDatabase :: [Film]
testDatabase = [
  (Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"]),
  (Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
  (Film "Body Of Lies" "Ridley Scott" 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
  (Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"]),
  (Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"]),
  (Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
  (Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"]),
  (Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"]),
  (Film "Prometheus" "Ridley Scott" 2012 ["Kevin", "Tim", "Emma", "Jo", "Liz"]),
  (Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"]),
  (Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"]),
  (Film "Jaws" "Steven Spielberg" 1975 ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"]),
  (Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"]),
  (Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"]),
  (Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"]),
  (Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"]),
  (Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"]),
  (Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"]),
  (Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"]),
  (Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"]),
  (Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"]),
  (Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"]),
  (Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"]),
  (Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"]),
  (Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"])
  ]

--
-- Helper Functions
--

-- checks if a film is above a certain year
checkFilm :: Year -> Film -> Bool
checkFilm y (Film title director year fans)
          | y < year = True
          | otherwise = False

-- checks if fans name is in a films fan list
isFan :: String -> Film -> Bool
isFan fanName (Film title director year fans)
          | elem fanName fans = True
          | otherwise = False

--
--  Functional code
--

--1 add a new film to the database
addFilm :: Title -> Director -> Year -> [Film] -> [Film]
addFilm title director year database = (Film title director year []) : database

--2 give all films in the database
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((Film title director year fans):xs) = title ++ " by " ++ director ++
                                                    ", released " ++  (show year)
                                                    ++ ".\nThe fans for this film are: "
                                                    ++ (show fans) ++ ".\n\n" ++ filmsAsString xs

--3 give all the films that were released after a particular year (not including the given year)
displayFilmsAfterYear :: Year -> [Film] -> String
displayFilmsAfterYear year database = filmsAsString(filter(checkFilm year) database)

--4 give all films that a particular user is a fan of
filmsByFan :: String -> [Film] -> String
filmsByFan fanName database = filmsAsString(filter(isFan fanName) database)

--5 give all the fans of a particular film
fansOfFilm :: Title -> [Film] -> [[String]]
fansOfFilm name database = [fans|Film title director year fans <- database, name == title] 

--6 allow a user to say they are a fan of a particular film
addFan :: String -> Title -> [Film] -> [Film]
addFan fan filmTitle [] = []
addFan fan filmTitle ((Film title director year fans) : xs)
          | (filmTitle == title) && not(isFan fan (Film title director year fans))
              = (Film title director year fan : fans) : addFan fan filmTitle xs
          | otherwise = (Film title director year fans) : addFan fan filmTitle xs
          
--7
--sameDirector

--getFilmsDirector








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
--demo 5 = putStrLn(fansOfFilm("Jaws" testDatabase))
--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
--demo 6 = putStrLn(filmsAsString(addFan "Liz" "The Fly" testDatabase))
--demo 66 = putStrLn(filmsAsString(addFan "Liz" "Avatar" testDatabase))
--demo 7 =  putStrLn all fans of films directed by "James Cameron"
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of

--
--
-- Your user interface code goes here
--
--
