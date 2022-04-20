#!/bin/bash
for i in {1..20}
  do
    echo "out_$i.smt2"
    bash b.sh "smt/out_$i.smt2"
  done
 
  