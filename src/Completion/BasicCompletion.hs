module Completion.BasicCompletion (
    complete
) where

import TermRewriting.Rewrite    ( RewriteRule(..), RewriteSystem(..) )
import Terms.Terms              ( OrderedSig, Term(..) )
import Confluence.CriticalPairs ( CriticalPair(..) )

data CompletionEnvironment 
    = Env { 
        symbolOrdering :: OrderedSig, 
        criticalPairs :: [CriticalPair], 
        rewriteSystem :: RewriteSystem, 
        equations :: [(Term, Term)]
    } deriving (Show)

complete :: RewriteSystem -> Maybe RewriteSystem
complete = undefined 