{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- legally this has to be in a different module

module Game.TH
  ( defComponent
  , mkType
  , localComponentNames
  , componentNames
  , mkDelete
  , mkListComponents
  , pb
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
mkType = mkType' . ConT where
  mkType' type' []       = type'
  mkType' type' (a : as) = mkType' (AppT type' $ ConT a) as


localComponentNames :: [Name]
localComponentNames =
  [ mkName "CanMove"
  , mkName "HasHealth"
  , mkName "HasLocation"
  , mkName "IsDoor"
  , mkName "IsWall"
  , mkName "IsFire"
  , mkName "IsPortal"
  , mkName "IsPotion"
  , mkName "IsStaircase"
  , mkName "IsEvil"
  , mkName "IsPlayer"
  ]

componentNames :: [Name]
componentNames =
  [ mkName "Message"
    , mkName "InGameStage"
    , mkName "TurnsElapsed"
    , mkName "SecsElapsed"
    , mkName "Depth"
    ]
    <> localComponentNames

mkDelete :: [Name] -> Q [Dec]
mkDelete compNames = (: []) <$> do
  let comps = map conT compNames
      stmts = map (\c -> noBindS
                    [e|
                      whenM (exists entity (Proxy :: Proxy $c)) $
                        destroy entity (Proxy :: Proxy $c)
                    |]
                  ) comps
  funD (mkName "delete")
       [clause [varP $ mkName "entity"] (normalB $ doE stmts) []]

-- FOR DEBUGGING
mkListComponents :: Q [Dec]
mkListComponents = (: []) <$> do
  let comps = map (\c -> (conT c, (litE . stringL . nameBase) c)) localComponentNames
      stmts = map (\(ct, cn) -> noBindS
                    [e|
                      whenM (exists entity (Proxy :: Proxy $ct)) $
                        liftIO $ putStrLn ("Component " <> $cn)
                    |]
                  ) comps
  funD (mkName "listComponents") [clause [varP $ mkName "entity"] (normalB $ doE stmts) []]


pb :: String -> Q Exp
pb name = appE (conE $ mkName "P") $ sigE
  (conE $ mkName "Proxy")
  (appT (conT $ mkName "Proxy") (conT $ mkName name))
