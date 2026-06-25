#!/usr/bin/awk -f

BEGIN {
    printf("sex\tage\tuc\tent\n")
}

{
    sex = substr($0,35,1)
    agedet = substr($0,39,3)
    uc = substr($0,60,4)
    ent = substr($0,101,112)
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
    printf("%s\t%d\t%s\t", sex, age, uc)
    for (i = 1; i <= length(ent); i = i+8) printf("%s#", substr(ent,i,8))
    printf("\n")
}
