import Development.Cake

main = cake $ do
  "build/A.txt" *> \out -> do
    copy "A.txt" out
  want ["build/A.txt"]