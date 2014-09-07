import Churro.Interpreter

main :: IO ()
main =
    do{ let code = "{o}===} {==={*} {*}=} {={o} {===={*}"
      ; interpretParse code
      }