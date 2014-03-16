module Language.Simple (
  module Language.Simple.AST,
  module Language.Simple.Values,
  interpret
) where

import Language.Simple.Compiler
import Language.Simple.Backends.Evaluator
import Language.Simple.AST
import Language.Simple.Values

interpret :: SimpleExpr -> SimpleValue
interpret = evalTop . compile
