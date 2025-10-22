module FML.Grammar where

data FMLState = FMLState String String deriving (Show)

data Attribute = Attribute String String deriving (Show)

data FMLElement = FMLElement String [Attribute] [FMLElement] | FMLLiteral String deriving (Show)

data FMLExpr
  = -- Declarations
    -- ~ count: number = 0
    -- \^   ^      ^     ^
    -- \|   |      |     |
    -- \|   |      |     +-> Default Value
    -- \|   |      +-> Type Declaration
    -- \|   +-> Identifier
    --  +-> FML Operator
    SignalDeclaration String String String
  | AsyncSignalDeclaration String String String
  | StoreDeclaration String String String

data ScriptExpr = JSExpr String | FMLExpr deriving (Show)

data FML
  = FMLComponent String FMLElement
  | FMLScript [ScriptExpr]
  | FMLStyle String String
  deriving (Show)

data ParseError = ParseError String Int Int deriving (Show, Eq)

printParseError :: ParseError -> String -> String
printParseError (ParseError msg row col) input =
  let errorLine = lines input !! (row - 1)
      pointer = replicate (col - 1) ' ' ++ "^"
   in unlines
        [ "Parse error at line " ++ show row ++ ", column " ++ show col ++ ":",
          errorLine,
          pointer,
          "Message: " ++ msg
        ]

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq)
