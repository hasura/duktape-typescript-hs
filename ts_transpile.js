//// Adapted from 
//// https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API#a-minimal-compiler

// Use our shim from the host language to implement 'require':
Duktape.modSearch = hs_modsearch;
const ts = require('typescript')

// TODO: this doesn't actually do any type-checking, but if we can dream it we can do itttttt:
//  https://stackoverflow.com/questions/53733138/how-do-i-type-check-a-snippet-of-typescript-code-in-memory 
function transpileTypeScript(source) {
    const result = ts.transpileModule(source, { 
                     compilerOptions: { 
                         // N.B. duktape only supports ES5:
                         target: ts.ScriptTarget.ES5,
                         module: ts.ModuleKind.CommonJS 
                     }});
    return result;
}
// const source = "let x: string  = 'string'";
// const result = ts.transpileModule(source, { 
//                  compilerOptions: { 
//                      // N.B. duktape only supports ES5:
//                      target: ts.ScriptTarget.ES5,
//                      module: ts.ModuleKind.CommonJS 
//                  }});
// console.log(JSON.stringify(result));
