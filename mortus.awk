#!/usr/bin/awk -f
#
BEGIN {
    printf("sex\tage\tuc\tent\n")
}

{
    sex = substr($0,69,1)
    age = substr($0,70,4)
    uc = substr($0,146,4)
    ent = substr($0,165,140)
    sub(/ /, "", uc)
    sub(/[ ]+ $/, "", ent)
    printf("%s\t%d\t%s\t%s\n", sex, age, uc, ent)
}
