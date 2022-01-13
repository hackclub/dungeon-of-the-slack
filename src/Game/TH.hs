{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- legally this has to be in a different module

module Game.TH
    ( defComponent
    , mkType
    ) where

import           Apecs
import           Relude                  hiding ( Type )

import           Language.Haskell.TH


defComponent :: Text -> [(Text, Type)] -> Name -> Q [Dec]
defComponent name args storage = do
    instance' <- [d|
      instance Component $component where
        type Storage $component = $storage' $component
      |]
    return (data' : instance')
     where
        mkName' = mkName . toString

        name' = mkName' name
        data' = DataD [] name' [] Nothing [RecC name' (noBang args)] []
        component = conT name'
        storage' = conT storage

        noBang = map (\(n, t) -> (mkName' n, Bang NoSourceUnpackedness NoSourceStrictness, t))

mkType :: Name -> [Name] -> Type
mkType = mkType' . ConT  where
    mkType' type' []       = type'
    mkType' type' (a : as) = mkType' (AppT type' $ ConT a) as
