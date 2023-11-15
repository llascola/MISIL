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
     return $ Seq cmd1' cmd2
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
binOp exp1 exp2 op =
  do e1 <- evalExp exp1 
     e2 <- evalExp exp2
     return (op e1 e2)

uOp :: MonadState m => Exp a -> (a -> a) -> m a
uOp exp op = 
  do e1 <- evalExp exp
     return (op e1)

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- Expresiones enteras
evalExp (Const nv)          = return nv  
evalExp (Var v)             = do {nv <- lookfor v; return nv}
evalExp (UMinus iexp)       = uOp iexp (\n -> -n) 
evalExp (Plus iexp1 iexp2)  = binOp iexp1 iexp2 (+)
evalExp (Minus iexp1 iexp2) = binOp iexp1 iexp2 (-)
evalExp (Times iexp1 iexp2) = binOp iexp1 iexp2 (*)
evalExp (Div iexp1 iexp2)   = binOp iexp1 iexp2 div  
--Expresiones Booleanas
evalExp BTrue               = return True
evalExp BFalse              = return False
evalExp (Not bexp)          = uOp bexp not
evalExp (Lt iexp1 iexp2)    = binOp iexp1 iexp2 (<)
evalExp (Gt iexp1 iexp2)    = binOp iexp1 iexp2 (>)
evalExp (And bexp1 bexp2)   = binOp bexp1 bexp2 (&&)
evalExp (Or bexp1 bexp2)    = binOp bexp1 bexp2 (||)
evalExp (Eq iexp1 iexp2)    = binOp iexp1 iexp2 (==)
evalExp (NEq iexp1 iexp2)   = binOp iexp1 iexp2 (/=)
--
evalExp (EAssgn var iexp)   = 
  do nv <- evalExp iexp
     update var nv
     return nv  
--
evalExp (ESeq iexp1 iexp2)  =
  do evalExp iexp1
     evalExp iexp2 