# KT
sim2.rateshift.age <- function(age, numbsim, lambda, mu, times, norm) {
    phy <- list()
    for (j in 1:numbsim) {
        temp <- sim2.rateshift(0, age, lambda, mu, times, norm)
        phy <- c(phy, list(temp))
    }
    phy
}
