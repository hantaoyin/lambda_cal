{-# LANGUAGE TemplateHaskell, CPP #-}

-- | QuasiQuoter for non-interpolated strings, texts and bytestrings.
--
-- The "s" quoter contains a multi-line string with no interpolation at all,
-- except that the leading newline is trimmed and carriage returns stripped.
--
-- @
-- {-\# LANGUAGE QuasiQuotes #-}
-- import Data.Text (Text)
-- import Data.String.QQ
-- foo :: Text -- "String", "ByteString" etc also works
-- foo = [s|
-- Well here is a
--     multi-line string!
-- |]
-- @
--
-- Any instance of the IsString type is permitted.
-- 
-- (For GHC versions 6, write "[$s||]" instead of "[s||]".)
--
module StringQQ (stringQQ) where
import GHC.Exts (IsString(..))
import Language.Haskell.TH.Quote

-- | QuasiQuoter for a non-interpolating IsString literal. The pattern portion is undefined.
stringQQ :: QuasiQuoter
stringQQ = QuasiQuoter ((\a -> [|fromString a|]) . trimLeadingNewline . removeCRs)
                       (error "Cannot use q as a pattern")
                       (error "Cannot use q as a type")
                       (error "Cannot use q as a dec")
    where
    removeCRs = filter (/= '\r')
    trimLeadingNewline ('\n':xs) = xs
    trimLeadingNewline xs = xs
