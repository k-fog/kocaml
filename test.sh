#!/bin/bash

assert() {
    expected="$1"
    input="$2"

    echo "$input" | ./kocaml > tmp.s
    cc -Wa,--noexecstack -o tmp tmp.s
    ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

assert  42 '42'
assert   3 '1+2'
assert   1 '3 - 2'
assert  20 '4*5'
assert   5 '10/2'
assert   2 '8-4-2'
assert   1 '8/4/2'
assert   7 '1+2*3'
assert   9 '(1+2)*3'
assert   3 '1+((4-3)*2)'
assert   3 '+3'
assert   3 '- -3'
assert   6 'let x = 3 in x + x'
assert  13 'let x = 1 + 2 in let y = x * 3 in y + 4'
assert  10 '(let x = 1 + 2 in x) + (let y = 3 in y + 4)'
assert   2 'let x = 1 in let x = 2 in x'
assert   4 'let f = fun x -> x + 1 in f 3'
assert   8 'let f = fun x -> let y = x + 1 in y * 2 in f 3'
assert   4 'let x = 10 in let f = fun x -> x + 1 in f 3'
assert   7 '(fun x -> x + 2) 5'
assert   1 'true'
assert   0 'false'
assert   1 '1 < 2'
assert   0 '1 > 2'
assert   1 '1 <> 2'
assert   0 '1 = 2'
assert   1 '2 <= 2'
assert   0 '1 >= 2'
assert   1 'if true then 1 else 2'
assert   2 'if false then 1 else 2'
assert   3 'if 1 < 2 then 3 else 4'
assert   5 '(if true then 2 else 3) + 3'
assert   7 'let f = fun x -> if x < 0 then 0 - x else x in f (-7)'
assert   1 'if true then if true then 1 else 2 else 3'
assert   2 'if true then if false then 1 else 2 else 3'
assert   3 'if false then if true then 1 else 2 else 3'
assert  55 'let rec fib = fun n -> if n <= 2 then 1 else (fib (n-1)) + (fib (n-2)) in fib 10'
assert 120 'let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5'
assert   0 'not true'
assert   1 'not false'
assert   0 'true && false'
assert   1 'true && true && true'
assert   1 'false || false || true'
assert   1 '1 < 2 && 3 < 4'
assert   3 '1 + if true then 2 else 3'
assert   3 '1 + let x = 2 in x'
echo "All tests passed!"
