module SymbolTable ( SymbolTable, symbolForDeclaration, symbolForExpression, insertDeclarationSymbol, insertExpressionSymbol, emptySymbolTable, constructSymbolTable, printSymbolTable ) where

import Data.Map ( Map, (!), empty, lookup, insert, keys )
import Prelude hiding ( lookup )
import Syntax ( Expression, Declaration )
import Symbol ( Symbol )


data SymbolNode
    = SDeclaration Declaration
    | SExpression Expression
    deriving (Eq, Ord, Show)

-- map with indirection, so that multiple nodes can point to the same symbol and we can change the symbol for all nodes at once
type SymbolTable = (Map SymbolNode Int, Map Int Symbol, Int) -- idxs, symbols, nextIdx

printSymbolTable :: SymbolTable -> IO ()
printSymbolTable (idxs, symbols, _) = do
    let idxList = keys idxs
    mapM_ printSingle idxList
    where printSingle node = putStrLn ("at index " ++ show idx ++ " is symbol " ++ show symbol ++ "\n")
            where
                (Just idx) = lookup node idxs
                (Just symbol) = lookup idx symbols
-- inner functions
getSymbol :: SymbolNode -> SymbolTable -> Maybe Symbol
getSymbol node (idxs, symbols, _) = 
    let 
        index = lookup node idxs
    in
        case index of
            Nothing -> Nothing
            Just idx -> lookup idx symbols

-- TODO insertSymbol should return the index as well
-- then we need another method insertWithIndex, otherwise it's not possible for multiple nodes to point to the same symbol...

insertSymbol :: SymbolNode -> Symbol -> SymbolTable -> SymbolTable
insertSymbol node symbol (idxs, symbols, nextIdx) = (new_idxs, new_symbols, nextIdx + 1)
    where
        new_idxs = insert node nextIdx idxs
        new_symbols = insert nextIdx symbol symbols

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
insertExpressionSymbol :: Expression -> Symbol -> SymbolTable -> SymbolTable
insertExpressionSymbol = insertSymbol . SExpression

emptySymbolTable :: SymbolTable
emptySymbolTable = (empty, empty, 0)

-- TODO add functions for modifying a symbol at a position indirectly!! i.e. setting the symbols type to something etc.

-- TODO add prelude definitions here
constructSymbolTable :: SymbolTable
constructSymbolTable = (empty, empty, 0)