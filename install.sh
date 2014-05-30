#!/bin/bash 

echo "Creating calcify executable..."
raco exe src/calcify.rkt
mv src/calcify bin/calcify
echo "SUCCESS"