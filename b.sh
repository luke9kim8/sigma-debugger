#!/bin/bash


solver="cvc4/cvc4 --lang=smt2.6"
orig="$1"
mut="mut.smt2"


rm mut.smt2 out1 out2 2> /dev/null
sed 's/set-logic ALL/set-logic QF_NIA/'  $orig > $mut

timeout -s 9 30 $solver  $orig >out1 || {
    echo "Error: invalid 2"
    exit 1
}
timeout -s 9 30 $solver  $mut >out2 || {
    echo "Error: invalid 3 "
    exit 1
}
if ! grep "^sat" out1 ; then
    echo "Error: out1 is not sat"
    exit 1
fi

if ! grep "^unknown" out2 ; then
    echo "Error: out2 is not unsat"
    exit 1
fi


echo "Correct! The size of a.smt2 is $(cat $1 | wc -c)."
exit 0
