-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- Both "Path.Posix" and "Path.Windows" provide the same interface. This
-- module will reexport the appropriate module for your platform.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}

#if defined(mingw32_HOST_OS)
module Path(module Path.Windows) where
import Path.Windows
#else
module Path(module Path.Posix) where
import Path.Posix
#endif

instance Read (Path Abs Dir) where
        readsPrec i r =   maybe []  (\res -> [(res, rem)] ) $ parseAbsDir x  
                where  [(x ::String , rem)] = readsPrec i r
instance Read (Path Abs File) where
        readsPrec i r =  maybe []  (\res -> [(res, rem)] ) $ parseAbsFile x  
                where  [(x ::String , rem)] = readsPrec i r
--                       mres = parseAbsFile x :: Maybe (Path Abs File)

instance Read (Path Rel Dir) where
        readsPrec i r =  maybe []  (\res -> [(res, rem)] ) $ parseRelDir x 
                where  [(x ::String , rem)] = readsPrec i r
instance Read (Path Rel File) where
        readsPrec i r =  maybe []  (\res -> [(res, rem)] ) $ parseRelFile x 
                where  [(x ::String , rem)] = readsPrec i r

