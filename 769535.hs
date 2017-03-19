import Data.List
import Data.List.Split
import Data.Char

--
-- MATHFUN
-- UP769535
--

data Film = Film String String Int [String]
    deriving (Eq, Show, Ord, Read)

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
checkFilm y (Film title director year fans)
          | y < year  = True
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
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm title director year database = (Film title director year []) : database

--2 give all films in the database
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString  ((Film title director year fans):xs) = title ++ " by " ++ director ++
                                                        ", released " ++  (show year)
                                                        ++ ".\nThe number of fans for this film is: "
                                                        ++ (show (length fans)) ++ ".\n\n"
                                                        ++ filmsAsString xs

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

--6 allow a user to say they are a fan of a particular film
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
getFansByDirector directorName database = unlines(nub(concat(map(\(Film title _ _ _)
                                        -> lines (fansOfFilm title database))(filter(\(Film _ director _ _)
                                        -> directorName == director) database))))

--8 list all directors (without duplicates), giving for each one the number of his/her films
-- a particular user is a fan of
{-
countFilmsByFanAsDirectors :: String -> [Film] -> String
countFilmsByFanAsDirectors fanName database = unlines(nub(map(\(Film _ director _ _)
                                            -> count(filmsByFan fans) database)(filter(\(Film _ _ _ fans)
                                            -> fanName == fans) database))
-}


--
-- UI helper functions
--
-- check if numbers are in string
checkIfNo :: String -> Bool
checkIfNo = any isDigit

-- check if numbers are in string
checkIfString :: String -> Bool
checkIfString = any isLetter

-- check if a director exists          
directorExist :: String -> [Film] -> Bool
directorExist directorName [] = False
directorExist directorName (x:xs)
        | directorName == getDirector x = True
directorExist director (x:xs) = directorExist director xs
-- get a director
getDirector :: Film -> String
getDirector (Film _ director _ _) = director

-- check if a film exists
filmExist :: String -> [Film] -> Bool
filmExist filmName [] = False
filmExist filmName (x:xs)
        | filmName == getFilm x = True
filmExist title (x:xs) = filmExist title xs
-- get a film
getFilm :: Film -> String
getFilm (Film title _ _ _) = title

-- reads from films.txt
readDatabase :: IO [Film]
readDatabase = do
    contents <- readFile "films.txt"
    return (read contents :: [Film])
    
-- save back to files.txt
writeToDatabase :: [Film] -> IO ()
writeToDatabase newDatabase = do
    putStrLn "Saving changes..."
    writeFile "films.txt" (show newDatabase)
    putStrLn "Done"
    return ()
--
-- User Interface
--
main :: IO ()
main = do
    contents <- readDatabase
    putStrLn (filmsAsString(contents))
    putStrLn "\nPlease enter your name: "
    username <- getLine
    menu contents username
    return ()    

menu :: [Film] -> String -> IO ()
menu database username = do
    putStrLn "What would you like to do? Type in the number for your choice.\n"
    putStrLn "1. Add a new film to the database."
    putStrLn "2. Give all films in the database."
    putStrLn "3. Give all the films that were released after a particular year."
    putStrLn "4. Find all the films you are a fan of."
    putStrLn "5. Give all the fans of a particular film."
    putStrLn "6. Add yourself as a fan to a film."
    putStrLn "7. Give all the fans of a particular director."
    putStrLn "8. List all directors, giving for each, one number of their films that you are a fan of."
    putStrLn "9. Exit\n"
    line <- getLine
    case line of
        "1" -> choice1 database username
        "2" -> choice2 database username
        "3" -> choice3 database username
        "4" -> choice4 database username
        "5" -> choice5 database username
        "6" -> choice6 database username
        "7" -> choice7 database username
        "8" -> do putStrLn "Sorry, this option is unavailable at this time.\n" 
                  menu database username
        "9" -> do
                writeToDatabase database
                return ()
        _ -> do putStrLn "Please input just one number"
                menu database username
        
--1 add a new film to the database
choice1 :: [Film] -> String -> IO ()
choice1 database username = do
    putStr "Enter Title: "
    title <- getLine
    let exists = filmExist title database
    if (exists == True || title == "") then do
        putStrLn "This film exists."
        choice1 database username
    else do
        if (title == "") then do
            putStrLn "Invalid input"
            choice1 database username
        else do
            putStrLn "Enter a Director: "
            director <- getLine
            let isNumber = checkIfNo director
            if (isNumber == True) then do
                putStrLn "A director cannot have a number in their name."
                choice1 database username
              else do
                putStrLn "Enter a Year: "
                year <- getLine
                let isString = checkIfString year
                let yr = read year :: Int
                if ((isString == True) || (yr >= 2018)) then do
                    putStrLn "Please enter the correct year of release in digits."
                    choice1 database username
                  else do
                    putStrLn "Film successfully added to the database."
                    let newDatabase = addFilm title director yr database
                    menu newDatabase username

--2 give all films in the database
choice2 :: [Film] -> String -> IO ()
choice2 database username = do 
    if ((filmsAsString database) == []) then do
        putStrLn "There are no films in the database."
        menu database username
    else do        
        putStrLn(filmsAsString database)
        menu database username

--3 give all the films that were released after a particular year (not including the given year)
choice3 :: [Film] -> String -> IO ()
choice3 database username = do
    putStr "Enter a year: "
    year <- getLine
    let yr = read year :: Int
    let isString = checkIfString year
    if ((isString == True) || (yr >= 2018)) then do      
        putStrLn "Please enter a correct year in digits."
        choice3 database username
    else do
        putStrLn(displayFilmsAfterYear yr database)
        menu database username

--4 give all films that a particular user is a fan of
choice4 :: [Film] -> String -> IO ()
choice4 database username = do
    if ((filmsByFan username database) == []) then do
        putStrLn "You are not fans of any film.\n"
        menu database username
    else do
        putStrLn(filmsByFan username database)
        menu database username

--5 give all the fans of a particular film
choice5 :: [Film] -> String -> IO ()
choice5 database username = do
    putStr "Enter film title: "
    title <- getLine
    let exists = filmExist title database
    if ((not exists == True) || (title == "")) then do
        putStrLn "This doesn't film exist."
        menu database username
    else do
        putStrLn(fansOfFilm title database)
        menu database username

--6 allow a user to say they are a fan of a particular film
choice6 :: [Film] -> String -> IO ()
choice6 database username = do
    putStrLn "Enter a film title to add yourself as a fan to: "
    title <- getLine
    let exists = filmExist title database
    if (exists == True)
    then do
        let newDatabase = addFan username title database
        menu newDatabase username
    else do
        putStrLn "Sorry this film doesn't exist."
        choice6 database username

--7 give all the fans (without duplicates) of a particular director (i.e. those users who are
-- fans of at least one of the director’s films)
choice7 :: [Film] -> String -> IO ()
choice7 database username = do
    putStrLn "Enter a director's name: "
    director <- getLine
    if ((getFansByDirector director database) == []) then do
        putStrLn "Sorry, there are either no fans of this director, or the director does not exist."
        menu database username
    else do
        putStrLn(getFansByDirector director database)
        menu database username
 
  
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