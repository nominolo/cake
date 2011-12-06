import Development.Cake
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent ( threadDelay )
--import Control.Monad.Exception ( throwIO )

main = cake $ do
  "AB.txt" *> \out -> do
    liftIO $ threadDelay (10 * seconds)
    cat ["A.txt", "B.txt"] out

  "CD.txt" *> \out ->
     cat ["C.txt", "D.txt"] out

  "ABCD.txt" *> \out ->
     cat ["AB.txt", "CD.txt", "GH.txt"] out

  "EF.txt" *> \out ->
    fail "This rule is silly"

  "GH.txt" *> \out -> do
    cat ["A.txt", "AB.txt"] out
--    liftIO (readFile "mueanounaoe")
    return ()

  want ["ABCD.txt"]

seconds = 1000000
