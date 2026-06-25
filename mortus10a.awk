#!/usr/bin/awk -f
#
BEGIN {
    printf("sex\tage\tuc\tent\n")
}

{
    sex = substr($0,59,1)
    agedet = substr($0,64,3)
    uc = substr($0,142,4)
    ent = substr($0,162,140)
    if (agedet < 200)
        age = agedet
    else {
        if (agedet < 999)
            age = 0
        else
            age = 9999
    }
    sub(/ /, "", uc)
    sub(/[ ]+ $/, "", ent)
    printf("%s\t%d\t%s\t%s\n", sex, age, uc, ent)
}
