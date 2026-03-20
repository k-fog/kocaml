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
