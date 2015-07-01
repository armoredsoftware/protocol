#!/bin/bash
cabal sandbox init
cabal sandbox add-source ../rsa
cabal sandbox add-source ../../../../codeLocal/code/experiments/vchanTestCentOS_1/haskell/VChanUtil/
