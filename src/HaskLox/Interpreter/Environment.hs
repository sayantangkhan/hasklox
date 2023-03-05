module HaskLox.Interpreter.Environment (initializeEnvironment, enterScope, exitScope, lookupIdentifier, addIdentifier, identifierIsPresent, modifyIdentifier, Environment) where

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

lookupIdentifier :: ByteString -> Environment a -> IO (Maybe (Maybe a))
lookupIdentifier key environment = do
  scopes <- readIORef (environmentRef environment)
  findFirstOccurence scopes
  where
    findFirstOccurence :: [IORef (Scope a)] -> IO (Maybe (Maybe a))
    findFirstOccurence [] = return Nothing
    findFirstOccurence (x : xs) = do
      occurenceInx <- lookupIdentifierInCurrentScope x
      case occurenceInx of
        Just r -> return $ Just r
        Nothing -> findFirstOccurence xs
    lookupIdentifierInCurrentScope :: IORef (Scope a) -> IO (Maybe (Maybe a))
    lookupIdentifierInCurrentScope scopePointer = do
      scope <- readIORef scopePointer
      return $ M.lookup key (innerMap scope)

identifierIsPresent :: ByteString -> Environment a -> IO Bool
identifierIsPresent key environment = do
  scopes <- readIORef (environmentRef environment)
  findIdentifierInScopes scopes
  where
    findIdentifierInScopes :: [IORef (Scope a)] -> IO Bool
    findIdentifierInScopes [] = return False
    findIdentifierInScopes (x : xs) = do
      occurenceInX <- presentInScope x
      (if occurenceInX then return True else findIdentifierInScopes xs)
    presentInScope :: IORef (Scope a) -> IO Bool
    presentInScope scopePointer = do
      scope <- readIORef scopePointer
      return $ M.member key (innerMap scope)

addIdentifier :: ByteString -> Maybe a -> Environment a -> IO () -- Maybe fail if redefining variables outside of global scope
addIdentifier name possibleValue environment = do
  scopes <- readIORef (environmentRef environment)
  case scopes of
    [] -> return ()
    (scope : _) -> modifyIORef scope (Scope . M.insert name possibleValue . innerMap)

modifyIdentifier :: ByteString -> (Maybe a -> Maybe a) -> Environment a -> IO ()
modifyIdentifier key f environment = do
  scopes <- readIORef (environmentRef environment)
  modifyFirstOccurence f scopes
  where
    modifyFirstOccurence :: (Maybe a -> Maybe a) -> [IORef (Scope a)] -> IO ()
    modifyFirstOccurence _ [] = return ()
    modifyFirstOccurence g (x : xs) = do
      occurenceInX <- presentInScope x
      (if occurenceInX then modifyInScope g x else modifyFirstOccurence g xs)
    presentInScope :: IORef (Scope a) -> IO Bool
    presentInScope scopePointer = do
      scope <- readIORef scopePointer
      return $ M.member key (innerMap scope)
    modifyInScope :: (Maybe a -> Maybe a) -> IORef (Scope a) -> IO ()
    modifyInScope h scopePointer = do
      scope <- readIORef scopePointer
      let scopeMap = innerMap scope
      let newScope = Scope $ M.adjust h key scopeMap
      writeIORef scopePointer newScope
