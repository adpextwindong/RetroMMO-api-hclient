{-
(
Copyright (c) 2014, Zalora South East Asia Pte Ltd

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Zalora South East Asia Pte Ltd nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

This code is from https://github.com/axman6/servant/tree/file-extensions

Its been heavily modified to include basvandijk's idea of being polymorphic.

-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

module ServantContrib.API.FileExtension
    ( Ext(..)
    , getExt
    --, parseExt
    , renderExt
    , toExtProxy
    , liftExt
    ) where

import           Control.Monad   ((>=>))
import           Data.Maybe      (catMaybes)
import           Data.Proxy      (Proxy (..))
import           Data.Text       (Text, pack, stripSuffix)
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (Symbol, KnownSymbol, symbolVal)
import           Web.HttpApiData (ToHttpApiData (..), FromHttpApiData (..))
import           Data.Monoid     ((<>))
import           Data.Coerce     (Coercible, coerce)

-- | A wrapper around a `Text` value which must be suffixed by the extension `ext`.
--
-- Pattern matching on the `Ext` constructor will give you the `Text` value without
-- the suffix, use `renderExt` to retrive the full string including '.' and extension.
--
-- Example:
--
-- >>>            -- GET /file/:filename.png
-- >>> type MyApi = "file" :> Capture "filename" (Ext "png") :> Get '[JSON] Text
--
-- >>> let x = Ext "mypic" :: Ext "png"
-- >>> renderExt x
-- "mypic.png"
--
newtype Ext (ext :: Symbol) a = Ext {getBase :: a}
    deriving (Typeable, Eq, Ord)

{-
instance (KnownSymbol ext) => Show (Ext ext a) where
    showsPrec i tt@(Ext t) = showsPrec i (mappend t . pack $ '.':getExt tt)
-}

{-
instance (KnownSymbol ext) => Read (Ext ext a) where
    readsPrec i str = res
        where
            -- res :: [(Ext ext t, String)]
            res = catMaybes . map f . readsPrec (i+1) $ str

            f :: (Text,String) -> Maybe (Ext ext a,String)
            f (t,s) = fmap (\txt -> (Ext txt,s)) $ stripSuffix (pack $ '.':ext) t

            toExtTy :: [(Ext ext a, String)] -> Ext ext a
            toExtTy _ = undefined

            ext = getExt (toExtTy res)
-}


instance (KnownSymbol ext, ToHttpApiData a) => ToHttpApiData (Ext ext a) where
    toUrlPiece   = toUrlPiece   . renderExt
    toHeader     = toHeader     . renderExt
    toQueryParam = toQueryParam . renderExt

{-

TODO figure out FromHttpApiData

instance (KnownSymbol ext, ToHttpApiData a) => FromHttpApiData (Ext ext a) where
    parseUrlPiece   = parseUrlPiece   >=> parseExt
    parseHeader     = parseHeader     >=> parseExt
    parseQueryParam = parseQueryParam >=> parseExt
-}

toExtProxy :: Ext ext a -> Proxy ext
toExtProxy _ = Proxy

getExt :: KnownSymbol ext => Ext ext a -> String
getExt t = symbolVal (toExtProxy t)

{-
parseExt :: (KnownSymbol ext) => String -> Either Text (Ext ext a)
parseExt str = res
    where
        res = case stripSuffix (pack ext) (pack str) of
                Nothing -> Left . pack $ "The filename \"" ++ str ++ "\" does not end with extension \"" ++ ext ++ "\""
                Just txt -> Right (Ext txt)

        ext = '.':getExt (toExtTy res)

        toExtTy :: Either Text (Ext ext a) -> Ext ext a
        toExtTy _ = undefined
-}

renderExt :: (KnownSymbol ext, ToHttpApiData a) => Ext ext a -> Text
renderExt ee@(Ext t) = toUrlPiece t <> (pack $ '.':getExt ee)

liftExt :: (KnownSymbol ext, ToHttpApiData a) => a -> Ext ext a
liftExt = Ext

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
