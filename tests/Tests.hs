module Tests where

import Text.Garnett.Definition
import qualified Data.Map as Map

defaultBlock = Block { _progName    = Map.fromList [(defaultFmt, "test")]
                     , _authorName  = Map.fromList [(defaultFmt, "test name")]
                     , _authorEmail = Map.fromList [(defaultFmt, "test@email")]
                     , _shortDesc   = Map.fromList [(defaultFmt, "A test prog")]
                     , _completions = Map.fromList [(defaultFmt, comp)]
                     }
    where comp = Completions $ { _shortCompl = Map.fromList [("help", "h")]
                               , _longCompl  = Map.fromList [("help", "help")]
                               }

