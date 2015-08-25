#! /bin/bash

runhaskell ParseKannadaTB.hs
cd ../data
maltparser -c test -i kannada_train.conll -m learn
maltparser -c test -i kannada_test.conll -o out.conll -m parse
java -jar ../malteval-dist-20141005/lib/MaltEval.jar -g kannada_test.conll -s out.conll -v 0
cd ../code
