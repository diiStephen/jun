{-# LANGUAGE GADTs, StandaloneKindSignatures, DataKinds, 
    StandaloneDeriving, DerivingStrategies, ScopedTypeVariables, RankNTypes #-}

module Terms.TermsGADT () where

import Data.Kind   ( Type )
import Terms.Terms ( FSym )

data TermType = APP | VAR 

type Term2 :: TermType -> Type -> Type 
data Term2 t a where 
    V2 :: a -> Term2 VAR a 
    (:>) :: FSym -> [Term2 t a] -> Term2 APP a 
infixr 5 :>
deriving stock instance Show a => Show (Term2 t a)

type ExTerm :: Type -> Type
data ExTerm a where 
    MkET :: forall t a . Term2 t a -> ExTerm a
deriving stock instance Show a => Show (ExTerm a)

sym :: Term2 APP a -> FSym
sym (f :> _) = f

subterms :: Term2 APP a -> [ExTerm a]
subterms (_ :> ts) = map MkET ts