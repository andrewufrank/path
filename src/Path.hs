-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- Both "Path.Posix" and "Path.Windows" provide the same interface. This
-- module will reexport the appropriate module for your platform.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE OverloadedStrings   #-}

#if defined(mingw32_HOST_OS)
module Path(module Path.Windows) where
import Path.Windows
#else
module Path(module Path.Posix) where
import Path.Posix
#endif

import Data.String


instance IsString (Path Abs File) where
    fromString = read
instance IsString (Path Abs Dir) where
    fromString = read
instance IsString (Path Rel File) where
    fromString = read
instance IsString (Path Rel Dir) where
    fromString = read

instance Read (Path Abs Dir) where
        readsPrec i r =   maybe []  (\res -> [(res, rem1)] ) $ parseAbsDir x
                where  [(x ::String , rem1)] = readsPrec i r
instance Read (Path Abs File) where
        readsPrec i r =  maybe []  (\res -> [(res, rem1)] ) $ parseAbsFile x
                where  [(x ::String , rem1)] = readsPrec i r
--                       mres = parseAbsFile x :: Maybe (Path Abs File)

instance Read (Path Rel Dir) where
        readsPrec i r =  maybe []  (\res -> [(res, rem1)] ) $ parseRelDir x
                where  [(x ::String , rem1)] = readsPrec i r
instance Read (Path Rel File) where
        readsPrec i r =  maybe []  (\res -> [(res, rem1)] ) $ parseRelFile x
                where  [(x ::String , rem1)] = readsPrec i r

