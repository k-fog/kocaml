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

echo "All tests passed!"
