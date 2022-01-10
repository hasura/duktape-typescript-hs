#!/bin/bash

# This just pulls in the latest generated vendored typescript compiler js and
# its license

# TODO minify?
# The typescript API:
wget -q https://github.com/microsoft/TypeScript/raw/main/lib/typescript.js
# The declaration file appropriate for our ES5 target:
wget -q https://github.com/microsoft/TypeScript/raw/main/lib/lib.es5.d.ts
wget -q https://raw.githubusercontent.com/microsoft/TypeScript/main/LICENSE.txt

