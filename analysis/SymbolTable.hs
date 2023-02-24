module SymbolTable (SymbolTable, symbolForDeclaration, symbolForExpression, insertDeclarationSymbol, insertExpressionSymbol, emptySymbolTable, printSymbolTable, updateDeclarationSymbol, showSymbolTable) where

import Data.Foldable (foldl')
import Data.Map (Map, empty, insert, keys, lookup, (!))
import Symbol (Symbol (Symbol))
import Syntax (Declaration, Expression)
import Prelude hiding (lookup)

data SymbolNode
  = SDeclaration Declaration
  | SExpression Expression
  deriving (Eq, Ord, Show)

-- map with indirection, so that multiple nodes can point to the same symbol and we can change the symbol for all nodes at once
type SymbolTable = (Map SymbolNode Int, Map Int Symbol, Int) -- idxs, symbols, nextIdx

showSymbolTable :: SymbolTable -> String
showSymbolTable (idxs, symbols, nIdx) = foldl' line "" idxList
  where
    idxList = keys idxs
    line :: String -> SymbolNode -> String
    line string node = string ++ "at index " ++ show idx ++ " is symbol " ++ show symbol ++ "\n"
      where
        (Just idx) = lookup node idxs
        (Just symbol) = lookup idx symbols

printSymbolTable :: SymbolTable -> IO ()
printSymbolTable (idxs, symbols, _) = do
  let idxList = keys idxs
  mapM_ printSingle idxList
  where
    printSingle node = putStrLn ("at index " ++ show idx ++ " is symbol " ++ show symbol ++ "\n")
      where
        (Just idx) = lookup node idxs
        (Just symbol) = lookup idx symbols

-- inner functions
getSymbol :: SymbolNode -> SymbolTable -> Maybe Symbol
getSymbol node (idxs, symbols, _) =
  let index = lookup node idxs
   in case index of
        Nothing -> Nothing
        Just idx -> lookup idx symbols

insertSymbol :: SymbolNode -> Symbol -> SymbolTable -> SymbolTable
insertSymbol node symbol (idxs, symbols, nextIdx) = (new_idxs, new_symbols, nextIdx + 1)
  where
    new_idxs = insert node nextIdx idxs
    new_symbols = insert nextIdx symbol symbols

-- TODO change return value to Maybe
updateSymbol :: SymbolNode -> Symbol -> SymbolTable -> SymbolTable
updateSymbol node symbol (idxs, symbols, nextIdx) = (idxs, new_symbols, nextIdx)
  where
    (Just idx) = lookup node idxs
    new_symbols = insert idx symbol symbols

-- exported functions (mostly wrappers)
symbolForDeclaration :: Declaration -> SymbolTable -> Maybe Symbol
symbolForDeclaration = getSymbol . SDeclaration

symbolForExpression :: Expression -> SymbolTable -> Maybe Symbol
symbolForExpression = getSymbol . SExpression

insertDeclarationSymbol :: Declaration -> Symbol -> SymbolTable -> SymbolTable
insertDeclarationSymbol = insertSymbol . SDeclaration

insertExpressionSymbol :: Expression -> Symbol -> SymbolTable -> Maybe SymbolTable
insertExpressionSymbol expr symb@(Symbol _ _ decl _) (idxs, symbols, nextIdx) = do
  -- get index of of the symbol, by checking the entry of its declaration
  let declNode = SDeclaration decl
  symbIdx <- lookup declNode idxs
  -- insert new symbolNode pointing to the same index
  let exprNode = SExpression expr
  let newIdxs = insert exprNode symbIdx idxs
  return (newIdxs, symbols, nextIdx)

updateDeclarationSymbol :: Declaration -> Symbol -> SymbolTable -> SymbolTable
updateDeclarationSymbol = updateSymbol . SDeclaration

emptySymbolTable :: SymbolTable
emptySymbolTable = (empty, empty, 0)
