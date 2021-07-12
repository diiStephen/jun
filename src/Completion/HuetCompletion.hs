module Completion.HuetCompletion (

) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..) )
import Terms.Terms                ( Term(..) )
import TermRewriting.Rewrite      ( RewriteRule(..) )
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
    , rules :: [(Int,MarkedRule)]
}

type Log = [String] 

type CompletionEval a = ExceptT CompletionFailure (RWST TermOrder Log CompletionEnv Identity) a