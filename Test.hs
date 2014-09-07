import Churro.Parser

main :: IO ()
main = putStrLn $ show $ parseChurro "{o}=} {==={o} {o}==} {===={*} {o}==}"