

proc (name:date:_:points:rest)  = print [name,date,points]
proc _ = print "None"

main = do
    input <- readFile "data/2022-wtc.csv" 
    let fc = lines input
    proc fc
