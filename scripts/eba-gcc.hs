#!/usr/bin/runhaskell

import Control.Monad      ( void )
import Data.List          as L
import System.Directory   as Dir
import System.Environment ( getArgs )
import System.Exit        as Exit
import System.FilePath    as FP
import System.IO          as IO
import System.Process     as P

filterArgs :: [String] -> [String]
filterArgs [] = []
filterArgs (a:args)
	|  "-D" `L.isPrefixOf` a
	|| "-I" `L.isPrefixOf` a
	= a : filterArgs args
filterArgs (a1@"-include":a2:args)
	= a1 : a2 : filterArgs args
filterArgs (_skipped:args)
	= filterArgs args

testEbaCheckExists :: IO ()
testEbaCheckExists = do
	mb_found <- Dir.findExecutable "eba-check"
	case mb_found of
		Just _  -> return ()
		Nothing -> do
			IO.hPutStrLn IO.stderr "Cannot find your eba-check script!"
			Exit.exitFailure

-- | A very simple fake-GCC wrapper for EBA.
--  
-- Given a bunch of gcc options and a path/to/file.c:
--
-- 1. Filters gcc options to keep only those related to preprocessing.
-- 2. Calls gcc -E to preprocess path/to/file.c into _eba/path/to/file.c
-- 3. Calls a script named eba-check (that you must create) on _eba/path/to/file.c
--
-- The script eba-check should make the desired call to the EBA binary, usually
-- passing it --warn-output.
main :: IO ()
main = do
	testEbaCheckExists
	args <- getArgs
	let filepath = L.last args
	let (dir,file) = FP.splitFileName filepath
	let pp_dir = "_eba" FP.</> dir
	void $ P.rawSystem "mkdir" ["-p", pp_dir]
	let cpp_args = filterArgs args
	let pp_filepath = pp_dir FP.</> file
	let gcc_E_args = ["-E", "-o", pp_filepath] ++ cpp_args ++ [filepath]
	void $ P.rawSystem "gcc" gcc_E_args
	void $ P.rawSystem "eba-check" [pp_filepath]
