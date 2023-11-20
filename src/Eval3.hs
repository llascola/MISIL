module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a = StErTr {runStErTr :: Env -> Either Error (a, Env, Trace)}

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs
instance Monad StateErrorTrace where
  return a = StErTr (\s -> Right (a, s, ""))
  st >>= f = StErTr (\s -> case runStErTr st s of
                Left e -> Left e
                Right (a',s',t') ->
                    case runStErTr (f a') s' of
                      Left e -> Left e
                      Right (a'',s'',t'') -> Right (a'', s'', t' ++ t''))

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  write t = StErTr (\s -> Right ((), s, t))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StErTr (\s -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StErTr (\s -> lookfor' v s)
    where lookfor' v s =
            case M.lookup v s of
              Nothing -> Left UndefVar
              Just nv -> Right (nv, s, "")
  update v nv = StErTr (\s -> Right ((), M.insert v nv s, ""))


-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval cmd = case runStErTr (stepCommStar cmd) initEnv of
              Left e -> Left e
              Right (v, s, t) -> Right (s, t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm (Let v iexp) =
  do x <- evalExp iexp
     update v x
     write $ "Let " ++ v ++ "=" ++ show x ++ "\n"
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


-- Evalua una expresion
binOp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> Exp a -> (a -> a -> b) -> m b
binOp exp1 exp2 op =
  do e1 <- evalExp exp1 
     e2 <- evalExp exp2
     return $ op e1 e2

uOp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> (a -> a) -> m a
uOp exp op =
  do e1 <- evalExp exp
     return $ op e1

safeDiv :: (MonadState m, MonadError m, MonadTrace m) => Exp Int -> Exp Int -> m Int 
safeDiv exp1 exp2 =
  do e1 <- evalExp exp1
     e2 <- evalExp exp2
     if e2 == 0
     then throw DivByZero
     else return $ e1 `div` e2

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
-- Expresiones enteras
evalExp (Const  nv         ) = return nv  
evalExp (Var    v          ) = do {nv <- lookfor v; return nv}
evalExp (UMinus iexp       ) = uOp iexp (\n -> -n) 
evalExp (Plus   iexp1 iexp2) = binOp iexp1 iexp2 (+)
evalExp (Minus  iexp1 iexp2) = binOp iexp1 iexp2 (-)
evalExp (Times  iexp1 iexp2) = binOp iexp1 iexp2 (*)
evalExp (Div    iexp1 iexp2) = safeDiv iexp1 iexp2 
--Expresiones Booleanas
evalExp BTrue                = return True
evalExp BFalse               = return False
evalExp (Not    bexp       ) = uOp bexp not
evalExp (Lt     iexp1 iexp2) = binOp iexp1 iexp2 (<)
evalExp (Gt     iexp1 iexp2) = binOp iexp1 iexp2 (>)
evalExp (And    bexp1 bexp2) = binOp bexp1 bexp2 (&&)
evalExp (Or     bexp1 bexp2) = binOp bexp1 bexp2 (||)
evalExp (Eq     iexp1 iexp2) = binOp iexp1 iexp2 (==)
evalExp (NEq    iexp1 iexp2) = binOp iexp1 iexp2 (/=)
--
evalExp (EAssgn var iexp   ) = 
  do nv <- evalExp iexp
     update var nv
     write $ "Let " ++ var ++ "=" ++ show nv ++ "\n"
     return nv

evalExp (ESeq   iexp1 iexp2) =
  do evalExp iexp1
     evalExp iexp2 

