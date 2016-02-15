import GHC.Vis

main = do
  putStrLn "Start"
  let a = "teeest"
  let b = [1..3]
  let c = b ++ b
  let d = [1..]
  putStrLn $ show $ d !! 1

  vis
  view a "a"
  view b "b"
  view c "c"
  view d "d"

  getChar
  switch

  getChar
  putStrLn "End"
