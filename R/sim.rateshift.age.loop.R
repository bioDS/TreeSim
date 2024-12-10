# KT
sim.rateshift.age.loop <- function(
    age, numbsim, lambda, mu, times,
    mrca = FALSE, complete = TRUE, norm) {
    if (mrca == FALSE) {
        phy <- sim2.rateshift.age(age, numbsim, lambda, mu, times, norm)
        if (complete == FALSE) {
            phy <- reconstructed.age(phy, 1)
        }
    } else {
        if (complete == TRUE) {
            phy <- sim2.rateshift.mrca(age, numbsim, lambda, mu, times, norm)
        } else {
            phy <- sim2.rateshift.mrca(age, numbsim, lambda, mu, times, norm)
        }
    }
    phy
}
