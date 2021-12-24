{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, RecordWildCards #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Scripting.Duktape
import Control.Monad
import Data.Aeson
import GHC.Generics


-- | Initialise duktape with the enironment we want for client code. In this
-- case:
--
--   - module imports via 'require'
--   - console.log() etc.
createOurFancyDuktapeCtx = do
  Just ctx <- createDuktapeCtx  -- TODO in duktape: throw here;
  exposeFnDuktape ctx Nothing "hs_modsearch" hs_modsearch >>= \case
    Right () -> return ()
    Left err -> error err

  setupShims ctx

  return ctx
  where
    -- Here we specify in haskell how we should resolve module imports in JS
    hs_modsearch :: T.Text -> IO T.Text
    hs_modsearch = \case
      -- We could just read the file from the name client code provided, but this
      -- demonstrates whitelisting of modules:
      "typescript" -> do
          typescript_js <- T.readFile "typescript_vendored/typescript.js"
          return $ 
            -- typescript.js builds up a 'ts' object, but doesn't actually define a
            -- module by exporting it, so we do that here
            -- TODO is this the right way to do it, or am I dumb?
            typescript_js <> ";\n module.exports = ts;"

    -- Duktape doesn't implement anything that might require STDOUT/IN. Instead you
    -- implement this stuff in the host language.
    setupShims :: DuktapeCtx -> IO ()
    setupShims ctx = do
        ------ `console`
        evalDuktape ctx "var console = {}"
        -- In production we would collect these logs for each individual user,
        -- storing them in the DB, or perhaps streaming them to the console for a
        -- REPL view:
        exposeFnDuktape ctx (Just "console") "log" (\s -> putStrLn s >> return True) >>= \case -- TODO in duktape: fix overlapping instance
          Right () -> return ()
          Left err -> error err

main :: IO ()
main = do
  transpilerCtx <- createOurFancyDuktapeCtx

  putStrLn "*** Loading our typescript transpiler, which require()'s the typescript API JS"
  loadTypescriptCompiler transpilerCtx

  putStrLn "*** Transpiling a typescript program to ES5 JS, using our new transpiler"
  -- This would be untrusted user code, e.g. entered into a REPL in the
  -- hasura console, meant to power an action. In reality the function might
  -- take an object of a certain type we expose to client code, and also return
  -- a result of an expected shape (corresponding in some way to the user's
  -- schema, say):
  -- TODO a top-level 'function' declaration gives an error about missing 'find'...
  let userActionCodeTS = "const add: number = (x: number, y: number) =>  return x + y"
  Right (Just transpiledRes) <- callDuktape transpilerCtx Nothing "transpileTypeScript" [String userActionCodeTS]
  let Success TSTranspileOutput{..} = fromJSON transpiledRes
  putStrLn $ "    Niiiiiice: " <> T.unpack outputText

  let userActionCodeJS = T.encodeUtf8 outputText

  -- At this point we imagine we could store the user's transpiled JS text in
  -- the database (to power an action execution in the future), and we might
  -- also compile it into duktape bytecode for fast exection. Or we might just
  -- load it into a duktape ctx which we re-use (not clear whether that's a
  -- good idea, a security risk, etc.)
  putStrLn "*** Loading the user's transpiled function and executing it on an input"
  -- NOTE: in reality we'd use a different function to initialise context (e.g.
  -- obviously we don't need or necessarily want the client code itself to be
  -- able to compile typescript):
  executorCtx <- createOurFancyDuktapeCtx
  evalDuktape executorCtx userActionCodeJS >>= \case
    Left err -> error err
    Right (Just _val) -> error "We just defined a function here; nothing expected to be returned"
    Right Nothing -> return ()

  Right (Just actionRes) <- callDuktape executorCtx Nothing "add" [Number 20, Number 22]
  putStrLn $ "    Holy cow, did it work??: " <> show actionRes

  where
    -- Load our simple typescript compiler, which imports the module we created
    -- from 'typescript.js'
    --
    -- TODO: we might like to compile the typescript compiler to bytecode at
    -- haskell compile time, for security and possibly performance. We could
    -- also just store the source string at compile time too, obviously.
    loadTypescriptCompiler ctx = do
      ts_compiler_js <- B.readFile "ts_transpile.js"
      -- ts_compiler_js <- B.readFile "ts_compiler.js"

      -- guard B.isValidUtf8 ts_compiler_js  -- TODO in v0.11

      evalDuktape ctx ts_compiler_js >>= \case
        Left err -> error err
        Right (Just _val) -> error "We just defined a function here; nothing expected to be returned"
        Right Nothing -> return ()

-- | The output of ts.transpileModule, exposed via our transpileTypeScript
data TSTranspileOutput =
    TSTranspileOutput {
      diagnostics :: [String],
      -- ^ I'm not actually sure what shape this field expects
      outputText :: T.Text
    } deriving (Generic)
instance FromJSON TSTranspileOutput

