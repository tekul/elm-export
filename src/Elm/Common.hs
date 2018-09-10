{-# LANGUAGE OverloadedStrings #-}

module Elm.Common where

import Control.Monad.RWS
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Formatting hiding (text)
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

{- The options used when generating the Elm code.
   You can set the @fielLabelModifier@ to customize the names used for the
   fields in the generated Elm record types.
-}
data Options = Options
  { fieldLabelModifier :: Text -> Text
  }

{-| The default options which use the same record field names as the Haskell
code.
-}
defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}

cr :: Format r r
cr = now "\n"

mintercalate
  :: Monoid m
  => m -> [m] -> m
mintercalate _ [] = mempty
mintercalate _ [x] = x
mintercalate seperator (x:xs) = x <> seperator <> mintercalate seperator xs

pprinter :: Doc -> Text
pprinter = LT.toStrict . displayT . renderPretty 0.4 100

stext :: Data.Text.Text -> Doc
stext = text . LT.fromStrict

spaceparens :: Doc -> Doc
spaceparens doc = "(" <+> doc <+> ")"

--
type RenderM = RWS Options (Set Text -- The set of required imports
                            , [Text] -- Generated declarations
                            ) ()

{-| Add an import to the set.
-}
require :: Text -> RenderM ()
require dep = tell (S.singleton dep, [])

{-| Take the result of a RenderM computation and put it into the Writer's
declarations.
-}
collectDeclaration :: RenderM Doc -> RenderM ()
collectDeclaration =
  mapRWS ((\(defn, (), (imports, _)) -> ((), (), (imports, [pprinter defn]))))
