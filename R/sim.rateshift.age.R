# KT
sim.rateshift.age <- function(age, numbsim, lambda, mu, times, mrca = FALSE, complete = TRUE, norm = TRUE) {
    out <- lapply(1:numbsim, sim.rateshift.age.help,
        age = age, lambda = lambda, mu = mu, times = times,
        mrca = mrca, complete = complete, norm = norm
    )

    out
}
