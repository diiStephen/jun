module Completion.HuetCompletion (
    complete
) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..) )
import Terms.Terms                ( Term(..) )
import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..) )
import Equations.BasicEquation    ( Equation(..) )
import Control.Monad.RWS          ( RWST )
import Control.Monad.Except       ( ExceptT )
import Control.Monad.Identity     ( Identity )

data Mark a 
    = Marked a 
    | Unmarked a
    deriving (Show, Eq)

type MarkedRule = Mark RewriteRule

data CompletionEnv = Env {
      eqs :: [Equation Term Term]
    , rs :: [(Int,MarkedRule)] 
}

type Log = [String] 

type CompletionM a = ExceptT CompletionFailure (RWST TermOrder Log CompletionEnv Identity) a

complete :: [Equation Term Term] -> Maybe RewriteSystem 
complete = undefined 

eval :: [Equation Term Term] -> CompletionM RewriteSystem 
eval = undefined 

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env eqs []