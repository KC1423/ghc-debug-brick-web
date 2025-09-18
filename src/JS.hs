{-# LANGUAGE OverloadedStrings #-}

module JS where

import Data.Text
import Lucid
import Data.Functor.Identity (Identity)

{- Aliases for scripts to be included in Render -}
genScript :: Text -> HtmlT Identity ()
genScript loc = script_ [src_ loc, defer_ ""] (mempty :: Html ())
expandToggleScript :: HtmlT Identity () 
expandToggleScript = genScript "expandToggle.js"
selectTreeLinkScript :: HtmlT Identity () 
selectTreeLinkScript = genScript "selectTreeLink.js"
setSearchLimitScript :: HtmlT Identity () 
setSearchLimitScript = genScript "searchLimit.js"
takeSnapshotScript :: HtmlT Identity () 
takeSnapshotScript = genScript "takeSnapshot.js"
otherTextInputScript :: HtmlT Identity () 
otherTextInputScript = genScript "filterTextBox.js" 
