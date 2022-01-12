//// Adapted from 
//// https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API#a-minimal-compiler
//// https://stackoverflow.com/a/53764522/176841

// Use our shim from the host language to implement 'require':
Duktape.modSearch = hs_modsearch;
const ts = require('typescript')

// Compile typescript code (as utf8 string) to JS, returning the resulting
// transpiled code and any type errors.
function compileTypeScript(code) {
    // We treat the user's code as if it was entered into a file of this name.
    // We can easily extend this to support multiple "virtual" files.
    const dummyInputFileName  = "test.ts";
    const dummyOutputFileName = "test.js";
    // The compiler will "write out" the compiled JS to this variable, in 'writeFile':
    var outputCode = undefined;

    // Shims for "read file", "write file" etc. which we fake and implement only
    // as-needed (since we're compiling from strings in memory):
    var customCompilerHost = {
        getSourceFile: function (name) {
            var sourceFile;
            // console.log("getSourceFile ".concat(name));
            if (name === dummyInputFileName) {
                sourceFile = code;  // The code we're trying to compile
            }
            else {
                // Get any other file contents using a whitelist implemented in haskell:
                sourceFile = hs_ts_getSourceFile(name);
            }
            // NOTE: we target ES5 for duktape:
            // TODO do we actually want ES5 here instead of Latest? Not sure what this is for:
            return ts.createSourceFile(name, sourceFile, ts.ScriptTarget.Latest);
        },
        writeFile: function (name, data) {
            if (name === dummyOutputFileName) {
                return outputCode = data; 
            } else {
                console.log("Unexpected writeFile!: " + name);
            }
        },
        // of the declaration files included by lib.d.ts we only need this one
        // which we vendor.  If in the future we offer polyfills for features > ES5
        // then we can change this file accordingly:
        getDefaultLibFileName: function () { return "lib.es5.d.ts"; },

        // these are the minimal shims we need to avoid an error in the code
        // path we care about:
        useCaseSensitiveFileNames: function () { return true; },
        getCanonicalFileName: function (f) { return f; },
        getNewLine: function () { return "\n"; },
        // These don't seem to actually be called but need an implementation:
        getCurrentDirectory: function () { },
        readFile: function () { },
        //getDirectories: function () { return []; },
        //fileExists: function () { return true; },
    };
    const compilerOptions = {
        // N.B. duktape only supports ES5:
        target: ts.ScriptTarget.ES5,
        module: ts.ModuleKind.CommonJS,
        // TODO decide what kind of checking vs. vanilla JS support we want.
        // For now:
        strict: true,
    };
    var program = ts.createProgram([dummyInputFileName], compilerOptions, customCompilerHost);
    var emitResult = program.emit();
    var diagnostics = ts.getPreEmitDiagnostics(program);
    // Combine all diagnostics from all phases and simplify 'message' to a string:
    const allDiagnostics = diagnostics.concat(emitResult.diagnostics)
    allDiagnostics.forEach(function (d, _i) {
      d.messageText = ts.flattenDiagnosticMessageText(d.messageText, "\n");
      // NOTE: d.file has a ton of potentially-useful info. It's also a cyclic
      // structure, so we can't ever return it directly
      if (d.file !== undefined && d.file.hasOwnProperty('fileName')) {
          d.file = d.file.fileName
      }
      // To avoid additional issues with cyclic objects, etc. whitelist properties we want.
      // This should match `instance FromJSON TSCompilerDiagnostic`:
      const okKeys = [ "file", "start", "length", "messageText", "category", "code" ];
      Object.keys(d).forEach(function (k) { okKeys.indexOf(k) >= 0 || delete d[k] });
    })

    return {
        // TODO MAYBE: useful if we set `noEmitOnError: true`:
        // errored: emitResult.emitSkipped,
        jsCode: outputCode,
        diagnostics: allDiagnostics
    };
}
// TODO maybe useful
// let message = ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n");
