# KT
sim.rateshift.age.help <- function(dummy, age, lambda, mu, times, mrca = FALSE, complete = TRUE, norm) {
    out <- sim.rateshift.age.loop(age, 1, lambda, mu, times, mrca, complete, norm)[[1]]
    if (class(out) == "phylo" && mrca == FALSE) {
        out$root.edge <- age - max(getx(out, sersampling = 1)[, 1])
    }
    out
}
