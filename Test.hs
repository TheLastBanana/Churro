import Churro.Interpreter

main :: IO ()
main =
    do{ let code = "boogala boogala" ++ "{o}} {o}======} {o}} {====={*} {{o}" ++
                   "{==={o}" ++
                   "{o}==========} {={o}" ++
                   "boogala boogala" ++ 
                   "{o}} {======{o} {*}=} {={o} {o}} {====={*} {{o}" ++
                   "{===={*} {{o}" ++
                   "{o}=====} {={o} {========{o}" ++
                   "boogala boogala"
      ; interpretParse code ""
      }