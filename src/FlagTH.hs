{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module FlagTH (flagTH) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- |
-- @
-- flagTH [d|
--   data A = A {
--         a :: A,
--         b :: B,
--         c :: C,
--       }
--  |]
-- @
--
-- ======>
--
-- @
-- data A = A {
--       a :: A,
--       b :: B,
--       c :: C,
--     }
--
-- data AFlags = AFlags {
--       aFlag :: Maybe A,
--       bFlag :: Maybe B,
--       cFlag :: Maybe C,
--     }
--
-- combineAFlags (AFlags a b c) (AFlags a' b' c') =
--     AFlags (a `mplus` a') (b `mplus` b') (c `mplus` c')
--
-- unAFlags (AFlags (Just a) (Just b) (Just c)) =
--     A a b c
--
-- mkAFlags (A a b c) =
--     AFlags (Just a) (Just b) (Just c)
-- @
flagTH :: Q [Dec] -> Q [Dec]
flagTH qds = qds >>= \ds ->
  return $ concat $ map flagTH' ds

flagTH' :: Dec -> [Dec]
flagTH' (DataD ctx name@(Name (OccName n) _) tyvars cons dervs ) = [
    DataD ctx name tyvars cons dervs,
    DataD ctx (appendName "Flags" name) tyvars (map flagCon cons) dervs,
    FunD (mkName $ "combine" ++ n ++ "Flags") (map combineClause cons),
    FunD (mkName $ "un" ++ n ++ "Flags") (map unClause cons),
    FunD (mkName $ "mk" ++ n ++ "Flags") (map mkClause cons)
 ]
 where
   combineClause con = let
       cn = appendName "Flags" $ conName con
       args = flip take varNames $ nConArgs con
       a = map mkName args
       b = map mkName $ map (++"'") args
       p vn = ConP cn (map VarP vn)
     in
       Clause [p a, p b] (NormalB $ combineE cn a b) []

   unClause con = let
       cn = appendName "Flags" $ conName con
       args = map mkName $ flip take varNames $ nConArgs con
       e = foldl AppE (ConE (conName con)) $ map VarE args
       p = ConP cn (map (\n -> ConP 'Just [VarP n]) args)
     in
       Clause [p] (NormalB e) []

   mkClause con = let
       cn = appendName "Flags" $ conName con
       args = map mkName $ flip take varNames $ nConArgs con
       e = foldl AppE (ConE cn) $ map (\v -> AppE (ConE 'Just) (VarE v)) args
       p = ConP (conName con) (map VarP args)
     in
       Clause [p] (NormalB e) []

   mplusE (a,a') = InfixE (Just $ VarE a) (VarE 'mplus) (Just $ VarE a')

   combineE :: Name -> [Name] -> [Name] -> Exp
   combineE cn vns vns' =
       foldl (\e a -> AppE e (mplusE a)) (ConE cn) (vns `zip` vns')

varNames :: [String]
varNames = map (uncurry (++)) $ ls `zip` ns
 where
   ls = cycle $ map (:[]) ['a'..'z']
   ns = concatMap (replicate 26) ("" : map show [1..])

conName (NormalC cn stys)      = cn
conName (RecC cn vstys)        = cn
conName (InfixC sty cn sty')   = cn
conName (ForallC tyvars ctx c) = (conName c)

nConArgs = length . conArgs

conArgs (NormalC cn stys)      = stys
conArgs (RecC cn vstys)        = map (\(v,s,t) -> (s,t)) vstys
conArgs (InfixC sty cn sty')   = [sty, sty']
conArgs (ForallC tyvars ctx c) = (conArgs c)

flagCon (NormalC (appendName "Flags" -> cn) stys) =
    NormalC cn (map flagTyS stys)
flagCon (RecC (appendName "Flags" -> cn) vstys) =
    RecC cn (map flagTyVS vstys)
flagCon (InfixC sty (appendName "Flags" -> cn) sty') =
    InfixC (flagTyS sty) cn (flagTyS sty')
flagCon (ForallC tyvars ctx c) =
    ForallC tyvars ctx (flagCon c)

appendName str (Name (OccName n) _) = mkName (n ++str)

flagTyVS :: VarStrictType -> VarStrictType
flagTyVS (n, s, ty) = (appendName "Flag" n, s, flagTy ty)

flagTyS :: StrictType -> StrictType
flagTyS (s, ty) = (s, flagTy ty)

flagTy :: Type -> Type
flagTy ty = AppT (ConT ''Maybe) ty
