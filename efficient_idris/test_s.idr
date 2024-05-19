module Main

import S_combinator

main : IO ()
main = putStrLn $ show $ S (\x => x * 2) (\x => 3) 4
