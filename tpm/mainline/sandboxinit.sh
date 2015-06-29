#!/bin/bash
cabal sandbox init
cabal sandbox add-source ../rsa
cabal sandbox add-source ../editline/editline-0.2.1.1/
cabal sandbox add-source ../../../../codeLocal/code/experiments/vchanTestCentOS_1/haskell/VChanUtil/
cabal sandbox add-source ../shell/mainline/