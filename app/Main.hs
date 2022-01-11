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
import Control.Exception

-- | Initialise duktape with the enironment we want for client code. In this
-- case:
--
--   - module imports via 'require'
--   - console.log() etc.
--   - other shims needed in the TS compiler:
createOurFancyDuktapeCtx = do
  Just ctx <- createDuktapeCtx  -- TODO in duktape: throw here;
  -- for the TypeScript compiler:
  exposeFnDuktape ctx Nothing "hs_modsearch" hs_modsearch >>= throwLeft  -- TODO in duktape: throw here (probably? what are failure modes if global object?)
  exposeFnDuktape ctx Nothing "hs_ts_getSourceFile" hs_ts_getSourceFile >>= throwLeft

  setupShims ctx

  return ctx
  where
    -- Here we specify in haskell how we should resolve module imports in JS
    hs_modsearch :: T.Text -> IO T.Text
    hs_modsearch = \case
      -- We could just read the files from the name client code provided, but this
      -- demonstrates whitelisting of modules:
      "typescript" -> do
          typescript_js <- T.readFile "typescript_vendored/typescript.js"
          return $ 
            -- typescript.js builds up a 'ts' object, but doesn't actually define a
            -- module by exporting it, so we do that here
            -- TODO is this the right way to do it, or am I dumb?
            typescript_js <> ";\n module.exports = ts;"
      _ -> error "unexpected JS import"

    -- A host language shim to power getSourceFile in our TS compiler's
    -- CompilerHost implementation:
    hs_ts_getSourceFile :: T.Text -> IO T.Text
    hs_ts_getSourceFile = \case
      -- TS type declarations for ES5 std lib:
      "lib.es5.d.ts" -> T.readFile "typescript_vendored/lib.es5.d.ts"
      _ -> error "unexpected getSourceFile"

    -- Duktape doesn't implement anything that might require STDOUT/IN. Instead you
    -- implement this stuff in the host language.
    setupShims :: DuktapeCtx -> IO ()
    setupShims ctx = do
        ------ `console`
        Right _ <- evalDuktape ctx "var console = {}" -- TODO in duktape: maybe exposeFnDuktape should do this itself if undefined?
        -- In production we would collect these logs for each individual user,
        -- storing them in the DB, or perhaps streaming them to the console for a
        -- REPL view:
        -- TODO properly mimic console.log API:
        exposeFnDuktape ctx (Just "console") "log" (\s -> putStrLn s >> return True) >>= -- TODO in duktape: fix overlapping instance
          throwLeft

        -- ES6 Polyfills which the transpiled TypeScript API code needs, it seems:
        -- TODO I actually assumed these artifacts were using the ES5 target
        --      since things just worked at first, but it seems they actually
        --      use ES6! So I think we'll need to modify `typescript_vendored/setup.sh`
        --      to use ES5 and then run `gulp LKG` ourselves and copy the files.
        Right _ <- evalDuktape ctx $
            "(function () {" <>
            "    if (!Array.prototype.find) {" <>
            "      Object.defineProperty(Array.prototype, 'find', {" <>
            "        enumerable: false," <>
            "        configurable: true," <>
            "        writable: true," <>
            "        value: function(predicate) {" <>
            "          if (this == null) {" <>
            "            throw new TypeError('Array.prototype.find called on null or undefined');" <>
            "          }" <>
            "          if (typeof predicate !== 'function') {" <>
            "            throw new TypeError('predicate must be a function');" <>
            "          }" <>
            "          var list = Object(this);" <>
            "          var length = list.length >>> 0;" <>
            "          var thisArg = arguments[1];" <>
            "          var value;" <>
            "          for (var i = 0; i < length; i++) {" <>
            "            if (i in list) {" <>
            "              value = list[i];" <>
            "              if (predicate.call(thisArg, value, i, list)) {" <>
            "                return value;" <>
            "              }" <>
            "            }" <>
            "          }" <>
            "          return undefined;" <>
            "        }" <>
            "      });" <>
            "    }" <>
            "})();"
        return ()


main :: IO ()
main = do
  -- --------------------------------------------------------------------------
  -- This section is executed at action-creation time (e.g. in the API that
  -- powers a console editor pane for adding typescript actions)
  putStrLn "*** Loading our typescript transpiler, which require()'s the typescript API JS"
  transpilerCtx <- createOurFancyDuktapeCtx
  loadTypescriptCompiler transpilerCtx

  putStrLn "*** Transpiling a typescript program to ES5 JS, using our new transpiler"
  -- This would be untrusted user code, e.g. entered into a REPL in the
  -- hasura console, meant to power an action. In reality the function might
  -- take an object of a certain type we expose to client code, and also return
  -- a result of an expected shape (corresponding in some way to the user's
  -- schema, say):
  -- TODO a top-level 'function' declaration gives an error:
  --        TypeError: undefined not callable (property 'add' of [object global])
  -- TODO (re above) understand "eval code" vs "program code": 
  --      https://github.com/svaarala/duktape/issues/163#issuecomment-89756195 
  --      Maybe extend duktape bindings, document.
  -- let userActionCodeTS = "function add(x: number, y: number): number { return x + y }"
  let userActionCodeTS = "this.add = (x: number, y: number): number => { return x + y }"
  Right (Just transpiledRes) <- callDuktape transpilerCtx Nothing "compileTypeScript" [String userActionCodeTS]  -- TODO in duktape: consider taking [path, to, functionName] instead of (Maybe obj) here and in exposeFnDuktape

  let Success TSCompilerOutput{..} = fromJSON transpiledRes
  when (diagnostics /= []) $ do
      -- In reality this would get sent back from the API call, and e.g. displayed in the console:
      putStrLn "/===== We got some errors or warnings from TS compiler =====\\"
      printDiagnostics diagnostics
      putStrLn "\\===========================================================/"
  putStrLn $ "    Niiiiiice: " <> T.unpack jsCode

  -- --------------------------------------------------------------------------
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
  Right _ <- evalDuktape executorCtx (T.encodeUtf8 jsCode)
  -- ^ NOTE: this might or might not return a value; not clear if we want to enforce something here.

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
      ts_compiler_js <- B.readFile "ts_compiler.js"

      -- guard B.isValidUtf8 ts_compiler_js  -- TODO in v0.11 , maybe in evalDuktape directly

      evalDuktape ctx ts_compiler_js >>= \case
        Left err -> error err
        Right (Just _val) -> error "We just defined a function here; nothing expected to be returned"
        Right Nothing -> return ()


-- | The output of our compileTypeScript JS function
data TSCompilerOutput =
    TSCompilerOutput {
      diagnostics :: [TSCompilerDiagnostic],
      jsCode :: T.Text
    } deriving (Show, Eq, Ord, Generic)
instance FromJSON TSCompilerOutput

-- | Errors and warnings from the TypeScript compiler
data TSCompilerDiagnostic =
    TSCompilerDiagnostic {
      file :: Maybe T.Text,
      -- ^ TODO this seems to be "SourceFileObject" for the user's code for now...?
      start :: Maybe Int,
      -- ^ Source code location in bytes
      length :: Maybe Int,
      -- ^ Source code location span from 'start', in bytes
      messageText :: T.Text,
      category :: Int,
      code :: Int
      -- NOTE: we drop these for now, which always seem to come back undefined.
      --       There may be other properties for different error types too
      -- reportsUnnecessary :: undefined
      -- reportsDeprecated :: undefined
      -- relatedInformation: undefined
    } deriving (Show, Eq, Ord, Generic)
instance FromJSON TSCompilerDiagnostic


-- ==========================================================================
-- Boring stuff

throwLeft :: Monad m=> Either String a -> m a
throwLeft = either error return

printDiagnostics :: [TSCompilerDiagnostic] -> IO ()
printDiagnostics = mapM_ $ \TSCompilerDiagnostic{..} -> do
    putStr ">> "
    T.putStrLn messageText
    case (start, length) of
      (Just s, Just l) -> putStrLn $ "    ...at bytes "<>(show s)<>" - "<>(show $ l+s)
      _ -> return ()

