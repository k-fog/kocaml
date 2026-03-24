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

assert 42 '42'
assert  3 '1+2'
assert  1 '3 - 2'
assert 20 '4*5'
assert  5 '10/2'
assert  7 '1+2*3'
assert  9 '(1+2)*3'
assert  3 '1+((4-3)*2)'
assert  3 '+3'
assert  3 '- -3'
assert  6 'let x = 3 in x + x'
assert 13 'let x = 1 + 2 in let y = x * 3 in y + 4'
assert 10 '(let x = 1 + 2 in x) + (let y = 3 in y + 4)'
assert  2 'let x = 1 in let x = 2 in x'
assert  4 'let f = fun x -> x + 1 in f 3'
assert  8 'let f = fun x -> let y = x + 1 in y * 2 in f 3'
assert  4 'let x = 10 in let f = fun x -> x + 1 in f 3'
assert  7 '(fun x -> x + 2) 5'
assert  1 'true'
assert  0 'false'
assert  1 '1 < 2'
assert  0 '1 > 2'
assert  1 '1 <> 2'
assert  0 '1 = 2'
echo "All tests passed!"
