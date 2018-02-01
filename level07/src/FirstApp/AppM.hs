{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FirstApp.AppM where

import           Control.Applicative    (liftA2)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           FirstApp.DB.Types      (FirstAppDB)
import           FirstApp.Error         (Error)
import           FirstApp.Types         (Conf)

import           Data.Bifunctor         (first)

data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will expand the capabilities of our AppM by including the
-- Either type in our definition. We will also rework our Monad instance to stop
-- processing when it encounters a Left value.
--
-- This will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value.
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     alsoMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (Env -> IO (Either Error a))
  deriving Functor

-- The runAppM function only needs to change the final return type as it has an
-- 'Either Error' and not just the 'a'.
runAppM
  :: AppM a
  -> Env
  -> IO (Either Error a)
runAppM (AppM m) =
  m

-- Copy over your previously completed definitions.

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM (\_ -> (pure . pure) a)

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  -- Applicative f => f (a -> b) -> f a -> f b
  -- AppM (a -> b) :: Env -> IO (Either Error (a -> b))
  -- AppM a :: Env -> IO (Either Error a) - so we can <*> the IO instances
  -- IO (Either Error (a-b))
  (<*>) amFab ama = AppM (\e -> liftA2 (<*>) (runAppM amFab e) (runAppM ama e))

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  -- (Env -> IO (Either Error a)) -> (a -> (Env -> IO (Either Error b))) -> (Env -> IO (Either Error b))
  (>>=)  ama atMofB = AppM (\e -> do
    errOra <- runAppM ama e
    case errOra of
      Left err -> pure $ Left err
      Right a  -> runAppM (atMofB a) e)

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO ioOfa = AppM (\_e -> Right <$> ioOfa)

instance MonadReader Env AppM where
  ask :: AppM Env
  -- :: IO (Either Error Env)
  ask = AppM (pure . Right)

  local :: (Env -> Env) -> AppM a -> AppM a
  local f ama = AppM (runAppM ama . f)

  reader :: (Env -> a) -> AppM a
  reader f = AppM (pure . Right . f)

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  -- :: Error -> (Env -> IO (Either Error a))
  throwError e = AppM (\_ -> (pure . Left) e)

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError ama f = AppM (\env -> do
    errOra <- runAppM ama env
    case errOra of
      Left err -> runAppM (f err) env
      Right a  -> pure $ Right a)


-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither errOra = case errOra of
  Left e  -> throwError e
  Right a -> pure a
