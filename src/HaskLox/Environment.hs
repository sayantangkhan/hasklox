module HaskLox.Environment (initializeEnvironment, enterScope, exitScope, lookupIdentifier, addIdentifier, Environment) where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.IORef
import Data.Map qualified as M

newtype Scope a = Scope {innerMap :: M.Map ByteString (Maybe a)}

newtype Environment a = Environment {environmentRef :: IORef [IORef (Scope a)]}

initializeEnvironment :: IO (Environment a)
initializeEnvironment = do
  table <- newIORef $ Scope M.empty
  list <- newIORef [table]
  return $ Environment list

enterScope :: Environment a -> IO ()
enterScope environment = do
  table <- newIORef $ Scope M.empty
  modifyIORef (environmentRef environment) (table :)
  return ()

exitScope :: Environment a -> IO () -- Maybe have a runtime error type for this
exitScope environment = do
  modifyIORef (environmentRef environment) dropScope
  return ()
  where
    dropScope [] = []
    dropScope (_ : xs) = xs

lookupIdentifierInCurrentScope :: ByteString -> IORef (Scope a) -> IO (Maybe (Maybe a))
lookupIdentifierInCurrentScope key scopePointer = do
  scope <- readIORef scopePointer
  return $ M.lookup key (innerMap scope)

lookupIdentifier :: ByteString -> Environment a -> IO (Maybe (Maybe a))
lookupIdentifier key environment = do
  scopes <- readIORef (environmentRef environment)
  findFirstOccurence scopes
  where
    findFirstOccurence :: [IORef (Scope a)] -> IO (Maybe (Maybe a))
    findFirstOccurence [] = return Nothing
    findFirstOccurence (x : xs) = do
      occurenceInx <- lookupIdentifierInCurrentScope key x
      case occurenceInx of
        Just r -> return $ Just r
        Nothing -> findFirstOccurence xs

addIdentifier :: ByteString -> Maybe a -> Environment a -> IO () -- Maybe fail if redefining variables outside of global scope
addIdentifier name possibleValue environment = do
  scopes <- readIORef (environmentRef environment)
  case scopes of
    [] -> return ()
    (scope : _) -> modifyIORef scope (Scope . M.insert name possibleValue . innerMap)

modifyIdentifier :: ByteString -> (Maybe a -> Maybe a) -> Environment a -> IO ()
modifyIdentifier = undefined