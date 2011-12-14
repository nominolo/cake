import Development.Cake

main = cake $ do
  "AB.txt" *> \out -> do
    cat ["A.txt", "B.txt"] out
  "CD.txt" *> \out -> do
    cat ["C.txt", "D.txt"] out
  "ABCD.txt" *> \out -> do
    cat ["AB.txt", "CD.txt"] out
  want ["ABCD.txt"]
