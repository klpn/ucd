#!/usr/bin/awk -f

BEGIN {
    FS = "\t"
}

$3~/\|/ {
    div += 1
}

END {
    tot = NR - 1
    printf("div: %d tot: %d, rat: %f\n", div, tot, div/tot)
}
