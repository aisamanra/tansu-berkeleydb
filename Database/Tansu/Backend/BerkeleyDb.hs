module Database.Tansu.Backend.BerkeleyDb where

import Database.Berkeley.Db
import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)

import Database.Tansu.Internal (Database(..))

bdbSet :: Db -> ByteString -> ByteString -> IO ()
bdbSet db key val =
  db_put [] db Nothing key val

bdbGet :: Db -> ByteString -> IO (Maybe ByteString)
bdbGet db key =
  db_get [] db Nothing key

withBerkeleyDb :: FilePath -> (Database -> IO a) -> IO a
withBerkeleyDb path comp = do
  createDirectoryIfMissing True path
  env <- dbEnv_create []
  dbEnv_open [DB_CREATE,DB_INIT_MPOOL,DB_INIT_TXN] 0 env path
  db <- db_create [] env
  db_open [DB_CREATE] DB_HASH 0 db Nothing path Nothing
  result <- comp $ Database { dbGet = bdbGet db
                            , dbSet = bdbSet db
                            , dbRunTransaction = id
                            }
  db_close [] db
  dbEnv_close [] env
  return result
