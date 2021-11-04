{-# LANGUAGE GADTs, StandaloneKindSignatures, DataKinds, StandaloneDeriving, DerivingStrategies, ScopedTypeVariables, RankNTypes #-}

module Terms.TermsGADT () where

import Data.Kind   ( Type )
import Terms.Terms ( FSym )

data TermType 
    = APP 
    | VAR 

type Term :: TermType -> Type -> Type 
data Term t a where 
    V :: a -> Term VAR a 
    (:>) :: FSym -> [Term t a] -> Term APP a 
infixr 5 :>
deriving stock instance Show a => Show (Term t a)

type ExTerm :: Type -> Type
data ExTerm a where 
    MkET :: forall t a . Term t a -> ExTerm a
deriving stock instance Show a => Show (ExTerm a)

sym :: Term APP a -> FSym
sym (f :> _) = f

subterms :: Term APP a -> [ExTerm a]
subterms (_ :> ts) = map MkET ts