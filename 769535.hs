import Data.List
import Data.List.Split

--
-- MATHFUN
-- UP769535
--

--
-- Types
--
data Film = Film { fTitle :: String
                 , fDirector :: String
                 , fYear :: Int
                 , fFans ::[String]
                 } deriving (Eq, Show, Ord, Read)

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
checkFilm :: Int -> Film -> Bool
checkFilm y film
          | y < fYear film = True
          | otherwise = False

-- checks if fans name is in a films fan list
isFan :: String -> Film -> Bool
isFan fanName film
          | elem fanName (fFans film) = True
          | otherwise = False

-- modified version of Q2 but for directors only
directorsAsString :: [Film] -> String
directorsAsString [] = ""
directorsAsString ((Film _ director _ _):xs) = director ++ "\n" ++ directorsAsString xs

--
--  Functional code
--

--1 add a new film to the database
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm title director year database = (Film title director year []) : database

--2 give all films in the database 
-- try and output fans as a string format instead of a list
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString  ((Film title director year fans):xs) = title ++ " by " ++ director ++
                                                        ", released " ++  (show year)
                                                        ++ ".\nThe number of fans for this film is: "
                                                        ++ (show (length fans)) ++ ".\n\n" 
                                                        ++ filmsAsString xs
--filmsAsStringFans :: [Film] -> String
--filmsAsStringFans [] = ""
--filmsAsStringFans ((Film title director year fans):xs) = director ++ ".\n\n" ++ filmsAsStringFans xs

--3 give all the films that were released after a particular year (not including the given year)
displayFilmsAfterYear :: Int -> [Film] -> String
displayFilmsAfterYear year database = filmsAsString(filter(checkFilm year) database)

--4 give all films that a particular user is a fan of
filmsByFan :: String -> [Film] -> String
filmsByFan fanName database = filmsAsString(filter(isFan fanName) database)

--5 give all the fans of a particular film
fansOfFilm :: String -> [Film] -> String
fansOfFilm filmName database = unlines(concat[fans|Film title director year fans
                            <- database, filmName == title])

--6 allow a user to say they are a fan of a particular film changes to maybe?
addFan :: String -> String -> [Film] -> [Film]
addFan fanName filmName database = map(\(Film title director year fans) -> 
                                if(title == filmName) then 
                                (Film title director year (nub((fans++[fanName]))))
                                else (Film title director year fans)) database

--7 give all the fans (without duplicates) of a particular director (i.e. those users who are
-- fans of at least one of the director’s films)
-- lines splits string into list on every new line
-- unlines does the opposite of lines
-- concat joins subslists of a list together
-- nub removes duplicates
getFansByDirector :: String -> [Film] -> String
getFansByDirector directorName database = unlines(nub(concat(map(\(Film title _ _ _) -> lines (fansOfFilm title database))(filter(\(Film _ director _ _) -> directorName == director) database))))

--8 list all directors (without duplicates), giving for each one the number of his/her films
-- a particular user is a fan of
--countFilmsByFanAsDirectors :: String -> [Film] -> String
--countFilmsByFanAsDirectors fanName database = nub(map(\(Film _ director _ _)
--                                            -> count(filmsByFan fans) database)(filter(\(Film _ _ _ fans) 
--                                            -> fanName == fans) database))


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
demo 5 = putStrLn(fansOfFilm "Jaws" testDatabase)
--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
demo 6 = putStrLn(filmsAsString(addFan "Liz" "The Fly" testDatabase))
demo 66 = putStrLn(filmsAsString(addFan "Liz" "Avatar" testDatabase))
--demo 7 =  putStrLn all fans of films directed by "James Cameron"
demo 7 =  putStrLn(getFansByDirector "Ridley Scott" testDatabase)
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of


--
-- UI helper functions
--

-- rebuilds film database for outputting
buildFilm :: [[String]] -> [Film]
buildFilm (x:[]) = (Film (x !! 0) (x !! 1) (read (x !! 2):: Int) (splitFans((x !! 3)))) : []
buildFilm (x:xs) = (Film (x !! 0) (x !! 1) (read (x !! 2):: Int) (splitFans((x !! 3)))) : buildFilm xs

splitFans :: String -> [String]
splitFans fanList = splitOn "," ([x | x <- fanList, not(x `elem` " ")]) -- removes spaces and splits fans when ","

--
-- User Interface
--
main :: IO ()
main = do
    contents <- readFile "films.txt"
    let fileLines = lines([x | x <- contents, not(x `elem` "\"")])
    let rebuiltContents = buildFilm(splitWhen (=="") fileLines)
    putStrLn (filmsAsString(rebuiltContents))
    putStrLn "Please enter your name: "
    username <- getLine
    valChoice rebuiltContents username
    return ()
    
    {-
    let listWords = read contents :: [String]
    newList <- chooseAction listWords
    let listAsString = show newList
    writeFile "films.txt" (show listAsString)
    -}
valChoice :: [Film] -> String -> IO String
valChoice film username = do
    putStrLn "What would you like to do? Type in the number for your option."
    putStrLn "1. Add a new film to the database."
    putStrLn "2. Give all films in the database."
    putStrLn "3. Give all the films that were released after a particular year."
    putStrLn "4. Find all the films you are a fan of."
    putStrLn "5. Give all the fans of a particular film."
    putStrLn "6. Add yourself as a fan to a film."
    putStrLn "7. Give all the fans of a particular director."
    putStrLn "8. List all directors, giving for each, one number of their films that you are a fan of."
    putStrLn "9. Exit"
    line <- getLine
    case line of
        "1" -> return "1"
        "2" -> return "2"
        "3" -> return "3"
        "4" -> return "4"
        "5" -> return "5"
        "6" -> return "6"
        "7" -> return "7"
        "8" -> return "8"
        "9" -> return "9"
        _ -> do putStrLn "Please input just one number"
                valChoice film username
        {-
chooseAction ::  [String] -> IO [String]
chooseAction listWords = do
    choice <- valChoice
    if choice == "1" then do
        putStrLn "Type the word you want to add: "
        word <- getLine
        let newList = addWord word listWords
        chooseAction newList
    else if choice == "2" then do
        print listWords
        chooseAction listWords
    else if choice == "3" then do
        putStrLn "Type a length: "
        length <- getInt
        print (wordsOfLength length listWords)
        chooseAction listWords
    else
        return listWords
    -}
    
    
    





