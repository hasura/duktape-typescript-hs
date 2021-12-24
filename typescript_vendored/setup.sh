#!/bin/bash

# This just pulls in the latest generated vendored typescript compiler js and
# its license

# TODO minify?
wget -q https://github.com/microsoft/TypeScript/raw/main/lib/typescript.js
wget -q https://raw.githubusercontent.com/microsoft/TypeScript/main/LICENSE.txt

