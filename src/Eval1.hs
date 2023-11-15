module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm (Let v iexp) = 
  do x <- evalExp iexp
     update v x
     return Skip
stepComm (Seq Skip cmd2) = 
     return cmd2 
stepComm (Seq cmd1 cmd2) =
  do cmd1' <- stepComm cmd1
     return Seq cmd1' cmd2
stepComm (IfThenElse bexp cmd1 cmd2) =
  do b <- evalExp bexp     
     if b 
     then return cmd1
     else return cmd2
stepComm (While bexp cmd) =
  do b <- evalExp bexp
     if b
     then return $ Seq cmd (While bexp cmd)
     else return Skip

binOp :: MonadState m => Exp a -> Exp a -> (a -> a -> b) -> m b
binOP exp1 exp2 op =
  do e1 <- evalExp exp1 
     e2 <- evalExp exp2
     return (op e1 e2)

uOp :: MonadState m => Exp a -> (a -> a)
-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- Expresiones enteras
evalExp (Const nv) = return nv  
evalExp (Var v) =
  do nv <- lookfor v 
     return nv
evalExp (UMinus iexp) = 
  do nv <- evalExp iexp
     return (-nv)
evalExp (Plus iexp1 iexp2) =
  do n <- evalExp iexp1
     m <- evalExp iexp2
     return (n + m) 