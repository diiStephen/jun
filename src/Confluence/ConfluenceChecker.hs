module Confluence.ConfluenceChecker () where 

import TermRewriting.Rewrite  ( RewriteSystem(..) )
import Control.Monad.Identity ( Identity (runIdentity) )
import Control.Monad.Reader   ( ReaderT, runReaderT )
import Control.Monad.Writer   ( WriterT )

type ConfluenceEval a = ReaderT RewriteSystem (WriterT [String] Identity) a