import Development.Cake

main = cake $ do
  "AB.txt" *> \out ->
     cat ["A.txt", "B.txt"] out
  want ["AB.txt"]
