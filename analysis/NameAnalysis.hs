module NameAnalysis (Symbol, SymbolTable, symbolForDeclaration, symbolForExpression, emptySymbolTable, constructSymbolTable) where

import Data.Map (Map, (!), empty, lookup, insert)
import Prelude hiding (lookup)
import Syntax ( Expression, Declaration )

data Symbol = Symbol

data SymbolNode
    = SDeclaration Declaration
    | SExpression Expression
    deriving (Eq, Ord)

type SymbolTable = (Map SymbolNode Int, Map Int Symbol, Int) -- idxs, symbols, nextIdx


-- inner functions
getSymbol :: SymbolNode -> SymbolTable -> Maybe Symbol
getSymbol node (idxs, symbols, _) = 
    let 
        index = lookup node idxs
    in
        case index of
            Nothing -> Nothing
            Just idx -> lookup idx symbols
insertSymbol :: SymbolNode -> Symbol -> SymbolTable -> SymbolTable
insertSymbol node symbol (idxs, symbols, nextIdx) = (new_idxs, new_symbols, nextIdx + 1)
    where
        new_idxs = insert node nextIdx idxs
        new_symbols = insert nextIdx symbol symbols

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

-- TODO add prelude definitions here
constructSymbolTable :: SymbolTable
constructSymbolTable = (empty, empty, 0)