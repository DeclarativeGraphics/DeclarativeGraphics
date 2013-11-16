module Pretty where

import Text.PrettyPrint(Doc)

class Pretty p where
  pretty :: p -> Doc
