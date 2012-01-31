{-# OPTIONS -fglasgow-exts #-}
module Geography (World, Room, go, description, roomsFromString, Direction ( .. ) ) where
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

data Direction = North | South | West | East | NorthWest | SouthWest | NorthEast | SouthEast | Up | Down
     deriving (Eq, Ord, Show, Read, Enum)

newtype RoomId = RoomId Int deriving (Eq, Ord, Enum, Read, Show)

data Room a = Room { exits :: M.Map Direction RoomId, description :: String, extra :: Maybe a }


type World a = M.Map RoomId (Room a)

emptyRoom :: String -> Room a
emptyRoom = flip (Room M.empty) Nothing


oppositeDir :: Direction -> Direction
oppositeDir d = case d of
                  North -> South
                  South -> North
                  East -> West
                  West -> East
                  NorthWest -> SouthEast
                  SouthEast -> NorthWest
                  SouthWest -> NorthEast
                  NorthEast -> SouthWest
                  Up -> Down
                  Down -> Up


go :: Room a -> Direction -> World a -> Maybe (Room a)
go r d w = do a <- M.lookup d (exits r)
              b <- M.lookup a w
              return b

 
fromTriple :: World a -> M.Map String RoomId -> (String,String,[(Direction,String)]) -> World a
fromTriple w m (name,desc,exits) = case M.lookup name m of
                                     Nothing -> w
                                     Just i -> M.insert i (Room buildExits desc Nothing) w
                                 where buildExits = foldl build (M.empty) exits
                                       build acc (dir, k) = case M.lookup k m of
                                                              Nothing -> acc
                                                              Just v -> M.insert dir v acc 


data Declaration = Name String | Exit Direction String | Description String deriving (Eq, Show)

keywords, majorDirections, minorDirections, reservedWords :: [String]

keywords = ["room", "exit", "desc"] 


majorDirections = ["north", "south", "east", "west"]

minorDirections = do ns <- take 2 majorDirections
                     ew <- drop 2 majorDirections
                     return $ ns ++ ew
                     
reservedWords = keywords ++ majorDirections ++ minorDirections ++ ["up", "down"]

alpha, num, alphanum :: Parser Char

alpha = oneOf $ ['a'..'z'] ++ ['A'..'Z']
num = oneOf ['0'..'9']
alphanum = alpha <|> num

ident :: Parser String 
ident = do c <- (alpha <|> char '_')
           cs <- many alphanum
           spaces
           let s = c:cs
           if s `elem` reservedWords then 
              fail "Can't use a reserved word." 
              else return s

room, roomBody :: Parser [Declaration]
room = do string "room" 
          spaces
          body <- roomBody
          spaces
          return body

roomBody = do char '{'
              spaces 
              decls <- many roomDeclarations
              char '}'
              return decls

roomDeclarations, nameDeclaration, descriptionDeclaration, exitDeclaration :: Parser Declaration
roomDeclarations = do d <- (nameDeclaration <|> descriptionDeclaration <|> exitDeclaration)
                      spaces
                      return d

nameDeclaration = do string "name"
                     spaces
                     s <- ident
                     spaces
                     char ';'
                     return (Name s)

descriptionDeclaration = do string "desc"
                            spaces
                            s <- stringLit
                            spaces
                            char ';'
                            return (Description s)


exitDeclaration = do string "exit"
                     spaces
                     d <- direction
                     spaces
                     r <- ident
                     spaces
                     char ';'
                     return (Exit d r)

direction, prefixDir, northPrefix, southPrefix :: Parser Direction                            

direction = prefixDir <|> (string "east" >> return East) <|> (string "west" >> return West) <|>
            (string "up" >> return Up) <|> (string "down" >> return Down)
prefixDir = northPrefix <|> southPrefix
northPrefix = do string "north"
                 option North ((string "east" >> return NorthEast) <|> 
                               (string "west" >> return NorthWest))
southPrefix = do string "south"
                 option South ((string "east" >> return SouthEast) <|>
                               (string "west" >> return SouthWest))
stringLit :: Parser String 
stringLit = do char '"'
               s <- many stringChar
               char '"'
               return s

stringChar, escapedChar :: Parser Char
stringChar = noneOf ['\\', '"'] <|> escapedChar
escapedChar = do char '\\'
                 c <- anyChar
                 return $ case c of
                   'n' -> '\n'
                   'r' -> '\r'
                   '"' -> '"'
                   't' -> '\t'
                   'v' -> '\v'
                   ___ -> c

world :: Parser [[Declaration]]
world = do spaces
           many room
           
           

parseRooms :: String -> Either String [(String,String,[(Direction,String)])]

parseRooms input = case (parse world "" input) of
                     Left err -> Left $ show err
                     Right res -> Right $ map normalizeRoom res
                   where normalizeRoom decls = normRoom ("", "", []) decls
                         normRoom r [] = r
                         normRoom (name, desc, exits) (d:ds) = case d of
                                                                 Name s -> normRoom (s, desc, exits) ds
                                                                 Description s -> normRoom (name, s, exits) ds
                                                                 Exit dir s -> normRoom (name, desc, (dir, s):exits) ds

roomsFromString :: String -> Either String (Room a, World a)
roomsFromString rs = case (parseRooms rs) of
                          Left e -> Left e
                          Right r -> let w = foldl go (M.empty) r
                                         go acc t = fromTriple acc table t
                                         table = M.fromList $ zip (map (\(s,_,_) -> s) r) [RoomId 0..]
                                         in case M.lookup (RoomId 0) w of
                                                  Just r -> Right (r, w)
                                                  Nothing -> Left "Empty world."




