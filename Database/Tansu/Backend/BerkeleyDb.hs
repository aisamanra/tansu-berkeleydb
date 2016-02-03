module Database.Tansu.Backend.BerkeleyDb
         (withBerkeleyDb) where

import Control.Exception (catch)
import Database.Berkeley.Db
import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)

import Database.Tansu.Internal

catchIO :: IO a -> IO (Either TansuError a)
catchIO mote = fmap return mote `catch` go
  where go :: DbException -> IO (Either TansuError a)
        go = return . Left . OtherError . show

bdbSet :: Db -> ByteString -> ByteString -> IO (Either TansuError ())
bdbSet db key val =
  catchIO $ db_put [] db Nothing key val

bdbGet :: Db -> ByteString -> IO (Either TansuError ByteString)
bdbGet db key = do
  rs <- catchIO $ db_get [] db Nothing key
  case rs of
    Right Nothing  -> return (Left (KeyNotFound key))
    Right (Just x) -> return (return x)
    Left err       -> return (Left err)

bdbDel :: Db -> ByteString -> IO (Either TansuError ())
bdbDel db key =
  catchIO $ db_del [] db Nothing key

-- | Open or create a database at the supplied path
--   using the BerkeleyDB library. Right now, this uses
--   a consistent set of options, but probably should
--   become configurable at some point.
withBerkeleyDb :: FilePath -> (TansuDb k v -> IO a) -> IO a
withBerkeleyDb path comp = do
  createDirectoryIfMissing True path
  env <- dbEnv_create []
  dbEnv_open [DB_CREATE,DB_INIT_MPOOL,DB_INIT_TXN] 0 env path
  db <- db_create [] env
  db_open [DB_CREATE] DB_HASH 0 db Nothing path Nothing
  result <- comp $ TansuDb { dbGet = bdbGet db
                           , dbSet = bdbSet db
                           , dbDel = bdbDel db
                           , dbRunTransaction = id
                           }
  db_close [] db
  dbEnv_close [] env
  return result
