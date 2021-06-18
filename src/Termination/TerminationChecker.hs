module Termination.TerminationChecker (
    TerminationTactic,
    RewriteRule   (Rule),
    RewriteSystem (Rules),
    lpoTactic,
    mpoTactic,
    checkLpoTermination,
    checkMpoTermination,
    evalTermination
) where 

import Terms.Terms            ( Term(..), OrderedSig )
import Orders.TermOrders      ( lpo, mpo )
import Orders.PolyOrders      ( Order(..) )
import Data.List              ( all, intercalate )
import Control.Monad.Identity ( Identity (runIdentity) )
import Control.Monad.Except   ( ExceptT, throwError, MonadError (throwError), runExceptT )
import Control.Monad.Reader   ( ReaderT, runReaderT, ask, asks )
import Control.Monad.Writer   ( WriterT, runWriterT, tell )
import Control.Monad          (mplus)

newtype TerminationTactic = Tactic { runTactic :: Term -> Term -> Bool }
data RewriteRule          = Rule { lhs :: Term, rhs :: Term }
newtype RewriteSystem     = Rules { rules :: [RewriteRule] }

newtype TerminationError = TFail String 

type TerminationEval a = ReaderT OrderedSig (ExceptT TerminationError 
                                            (WriterT [String] Identity)) a

instance Show TerminationError where 
    show (TFail s) = "Could not prove termination for rule: " ++ s 

instance Show RewriteRule where 
    show r = show (lhs r) ++ " --> " ++ show (rhs r)

instance Show RewriteSystem where 
    show rs = "{" ++ intercalate " , " (map show (rules rs)) ++ "}" 

lpoTactic :: RewriteRule -> TerminationEval Bool 
lpoTactic rule = do 
    tell ["Checking: " ++ show (lhs rule) ++  " >_lpo " ++ show (rhs rule)]
    sig <- ask 
    case lpo sig (lhs rule) (rhs rule) of 
        GR -> return True 
        _  -> throwError (TFail ("Rule " ++ show rule ++ " cannot be proved terminating with lpo"))

mpoTactic :: RewriteRule -> TerminationEval Bool 
mpoTactic rule = do 
    tell ["Checking: " ++ show (lhs rule) ++ " >_mpo " ++ show (rhs rule)]
    sig <- ask 
    case mpo sig (lhs rule) (rhs rule) of 
        GR -> return True 
        _  -> throwError (TFail ("Rule " ++ show rule ++ " cannot be proved terminating with mpo"))

-- R is terminating iff \exists > , a reduction order on T(\Sigma, V) such that \forall l -> r \in R . l > r. 
-- For any (partial) order on \Sigma the induced lpo/mpo a simplification order, and therefore a reduction order. 
-- Note: The SAME reduction order must work for all rules in R. You cannot use differnt orders for each rule. 
-- This is due to the fact that termination is NOT a modular property of TRS's.  
checkLpoTermination :: RewriteSystem -> TerminationEval Bool
checkLpoTermination rs = do
    lpoAttempt <- mapM lpoTactic (rules rs)
    return (and lpoAttempt)

checkMpoTermination :: RewriteSystem -> TerminationEval Bool
checkMpoTermination rs = do 
    mpoAttempt <- mapM mpoTactic (rules rs)
    return (and mpoAttempt)

evalTermination :: (RewriteSystem -> TerminationEval Bool) -> OrderedSig -> RewriteSystem -> (Either TerminationError Bool, [String])
evalTermination strat sig trs = runIdentity (runWriterT (runExceptT (runReaderT (strat trs) sig)))