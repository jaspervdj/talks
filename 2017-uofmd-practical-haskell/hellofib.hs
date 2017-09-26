import Control.Parallel.Strategies
import System.Environment

fib 0 = 1
fib 1 = 1
fib n = runEval $ do
    x <- rpar (fib (n-1))
    y <- rseq (fib (n-2))
    return (x + y + 1)

main = do
   args <- getArgs
   n <- case args of
         []    -> return 20
         [x]   -> return (read x)
         _     -> fail ("Usage: hellofib [n]")
   print (fib n)
