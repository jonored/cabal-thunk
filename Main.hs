{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as BS
import qualified Data.Text as T
import Distribution.Fields
import Distribution.Parsec.Position (Position(..))
import Options.Applicative
import Data.Tuple
import System.Exit
import System.Process
import System.Directory
import qualified System.IO.Strict as Strict

doUnpack :: [String] -> [Field Position] -> String -> IO ()
doUnpack cfgStrs cfgFields a = do
  let [packages] = [fields | s@(Field (Name (Position _ _) "packages") fields) <- cfgFields]
      FieldLine (Position endPackagesLine _) _ = last packages
      [(stanza, uri)] = [(s, loc) | s@(Section (Name (Position _ _) "source-repository-package") [] fields) <- cfgFields, (Field (Name _ "location") [FieldLine _ loc]) <- fields, BS.encodeUtf8 (T.pack $ '/':a) `BS.isSuffixOf` loc]
      Section (Name (Position lineNo _) _) _ stanzaBody = stanza
      Field _ lastFieldLines = last stanzaBody
      FieldLine (Position endLineNo _) _= last lastFieldLines
      subdirs = [subdirText | Field (Name _ "subdir") s <- stanzaBody, FieldLine _ subdirText <- s]
      [tag] = [tagText | Field (Name _ "tag") s <- stanzaBody, FieldLine _ tagText <- s]
      (endLines, (_, (middleLines, startLines))) = fmap (swap . splitAt endPackagesLine) <$> ((swap . splitAt (lineNo-1)) <$> (swap . splitAt endLineNo) cfgStrs)
  writeFile "cabal.project" $ unlines $ join $
    [ startLines
    , ["-- Unpacked " <> a]
    , (("    " <> a <> "/") <>) <$> (T.unpack . BS.decodeUtf8 <$> subdirs)
    , middleLines
    , ["-- unpacked-from: " <> a ]
    , endLines
    ]
  homeDir <- getHomeDirectory
  repoExists <- doesDirectoryExist $ homeDir <> "/.git-dirs/" <> a
  repoDir <- canonicalizePath $ homeDir <> "/.git-dirs/" <> a
  unpackedDir <- canonicalizePath $ a
  if repoExists then return () else callProcess "git" ["clone", "--bare", T.unpack . BS.decodeUtf8 $ uri, repoDir]
  callProcess "git" ["--git-dir=" <> repoDir, "fetch", T.unpack . BS.decodeUtf8 $ uri, T.unpack . BS.decodeUtf8 $ tag]
  callProcess "git" ["--git-dir=" <> repoDir, "worktree", "add", "--detach", unpackedDir, "FETCH_HEAD"]
  --callProcess "git" ["clone", T.unpack . BS.decodeUtf8 $ uri]
  --cwd <- getCurrentDirectory
  --let repoDir = ((cwd <> "/") <>) $ a
  --void $ createProcess $ (proc "git" ["checkout", T.unpack . BS.decodeUtf8 $ tag]) { cwd = Just repoDir }

doPack :: [String] -> [Field Position] -> String -> IO ()
doPack cfgStrs cfgFields a = do
  (ExitSuccess, "", "") <- readCreateProcessWithExitCode ( (proc "git" ["status", "--porcelain"]) { cwd = Just a } ) ""
  return ()
  {-
  let [packages] = [fields | s@(Field (Name (Position _ _) "packages") fields) <- cfgFields]
      FieldLine (Position endPackagesLine _) _ = last packages
      [(stanza, uri)] = [(s, loc) | s@(Section (Name (Position _ _) "source-repository-package") [] fields) <- cfgFields, (Field (Name _ "location") [FieldLine _ loc]) <- fields, BS.encodeUtf8 (T.pack $ '/':a) `BS.isSuffixOf` loc]
      Section (Name (Position lineNo _) _) _ stanzaBody = stanza
      Field _ lastFieldLines = last stanzaBody
      FieldLine (Position endLineNo _) _= last lastFieldLines
      subdirs = [subdirText | Field (Name _ "subdir") s <- stanzaBody, FieldLine _ subdirText <- s]
      [tag] = [tagText | Field (Name _ "tag") s <- stanzaBody, FieldLine _ tagText <- s]
      (endLines, (_, (middleLines, startLines))) = fmap (swap . splitAt endPackagesLine) <$> ((swap . splitAt (lineNo-1)) <$> (swap . splitAt endLineNo) cfgStrs)
  writeFile "cabal.project" $ unlines $ join $
    [ startLines
    , ["-- Unpacked " <> a]
    , (("    " <> a <> "/") <>) <$> (T.unpack . BS.decodeUtf8 <$> subdirs)
    , middleLines
    , ["-- unpacked-from: " <> a ]
    , endLines
    ]
  homeDir <- getHomeDirectory
  repoExists <- doesDirectoryExist $ homeDir <> "/.git-dirs/" <> a
  repoDir <- canonicalizePath $ homeDir <> "/.git-dirs/" <> a
  unpackedDir <- canonicalizePath $ a
  if repoExists then return () else callProcess "git" ["clone", "--bare", T.unpack . BS.decodeUtf8 $ uri, repoDir]
  callProcess "git" ["--git-dir=" <> repoDir, "fetch", T.unpack . BS.decodeUtf8 $ uri, T.unpack . BS.decodeUtf8 $ tag]
  callProcess "git" ["--git-dir=" <> repoDir, "worktree", "add", "--detach", unpackedDir, "FETCH_HEAD"]
  -}

parser cfgStrs cfgFields = subparser (
  command "unpack" (info (doUnpack cfgStrs cfgFields <$> strArgument (metavar "REPO")) (progDesc "Unpack the dependency"))
  <>
  command "pack" (info (doPack cfgStrs cfgFields <$> strArgument (metavar "REPO")) (progDesc "Pack the dependency"))
  )

main :: IO ()
main = do
  cfgFile <- BS.readFile "cabal.project"
  cfgString <- Strict.readFile "cabal.project"
  let Right stanzas = readFields cfgFile
  join $ execParser (info (parser (lines cfgString) stanzas) (progDesc "Manage cabal dependencies like nix-thunk"))
