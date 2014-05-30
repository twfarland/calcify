#!/bin/bash 

echo "Creating calcify executable..."
raco exe src/calcify.rkt
mkdir bin
mv src/calcify bin/calcify
echo "SUCCESS"