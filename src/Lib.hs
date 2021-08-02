module Lib ( 
    entry
) where

import Interface.Repl ( repl )

entry :: IO ()
entry = repl