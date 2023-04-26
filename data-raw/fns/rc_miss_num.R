rc_miss_num <- function(df) {
 df[df == -1 |
    df == -2 |
    df == -3 |
    df == -4 |
    df == -5 |
    df == -6 |
    df == -7 |
    df == -8 |
    df == -9 |
    df == -10] <- NA
 return(df)
}


