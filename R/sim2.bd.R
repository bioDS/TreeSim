sim2.bd <-
    function(n, age, lambda, mu, K) {
        cat("sim2.bd call\n")
        phy2 <- sim2.bd.origin(n, age, lambda, mu, K)
        if (class(phy2) == "phylo") {
            # Tanja 9.7.: include root age!
            phy2 <- collapse.singles(phy2)
        }
        phy2
    }
