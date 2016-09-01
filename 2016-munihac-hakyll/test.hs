{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
import qualified Data.Map as M
type Identifier = String


data CompilerResult a
    = CompilerDone a
    | CompilerError String
    | CompilerRequire Identifier (Compiler a)
    deriving (Functor)

newtype Compiler a = Compiler {unCompiler :: IO (CompilerResult a)}
    deriving (Functor)


instance Applicative Compiler where

instance Monad Compiler where
    return = Compiler . return . CompilerDone

    (Compiler mx) >>= f = Compiler $ do
        res <- mx
        case res of
            CompilerDone  x -> unCompiler $ f x
            CompilerError e -> return $
              CompilerError e
            CompilerRequire i c -> return $
                CompilerRequire i (c >>= f)

data ScheduleState
  = Done
  | forall a. Paused (Compiler a)

type Scheduler =
  M.Map Identifier ScheduleState

runCompiler :: Compiler a -> IO a
runCompiler (Compiler c0) = do
  res <- c0
  case res of
      CompilerDone x -> return x
      CompilerError e -> fail e

schedule :: Identifier -> Scheduler -> IO Scheduler
schedule ident scheduler =
  case scheduler M.! ident of
    Done -> return scheduler
    Paused c0 -> do
      res <- unCompiler c0
      case res of
        CompilerDone _ ->
          -- Write to file...
          return (M.insert ident Done scheduler)
        CompilerError e ->
          fail e
        CompilerRequire i c1 ->
          schedule i (M.insert ident (Paused c1) scheduler)
