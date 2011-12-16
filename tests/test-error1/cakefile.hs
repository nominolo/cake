import Development.Cake

main = cake $ do
  "AB.txt" *> \out -> do
    cat ["A.txt", "B.txt"] out
  want ["AB.txt"]
