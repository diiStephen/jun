module Termination.TerminationChecker (
    TerminationTactic,
    TerminationError (TFail),
    lpoTactic,
    mpoTactic,
    checkLpoTermination,
    checkMpoTermination,
    evalTermination
) where 

import Terms.Terms            ( Term(..), OrderedSig )
import TermRewriting.Rewrite  ( RewriteRule(..), RewriteSystem(..) )
import Orders.TermOrders      ( lpo, mpo )
import Orders.PolyOrders      ( Order(..) )
import Data.List              ( all )
import Control.Monad.Identity ( Identity (runIdentity) )
import Control.Monad.Except   ( ExceptT, throwError, MonadError (throwError), runExceptT )
import Control.Monad.Reader   ( ReaderT, runReaderT, ask, asks )
import Control.Monad.Writer   ( WriterT, runWriterT, tell )

newtype TerminationTactic = Tactic { runTactic :: Term -> Term -> Bool }

newtype TerminationError 
    = TFail String 
    deriving (Eq)

type TerminationEval a = ReaderT OrderedSig (ExceptT TerminationError 
                                            (WriterT [String] Identity)) a

instance Show TerminationError where 
    show (TFail s) = "Could not prove termination: " ++ s 

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