## The structure of these conditional intensity functions (and the
## template assumed by other functions in the ptproc library) is
## modified from the Statistical Seismology Library (SSLIB) of Harte
## (1998).  See the man page for the complete reference.

linear.cond.int <- function(params, eval.pts, pts = NULL, data = NULL, TT = NULL) {
    mu <- params[1]
    beta <- params[-1]
    
    if(is.null(TT)) {
        ## Evaluate
        ci <- mu + eval.pts %*% beta
        ci <- as.vector(ci)
    }
    else {
        ## Integrate
        total.vol <- prod(apply(TT, 2, diff))
        m.vol <- sapply(1:ncol(TT), function(i)
        {
            z <- TT[,-i,drop=FALSE]
            prod(apply(z, 2, diff))
        })
        d <- apply(TT^2 / 2, 2, diff)
        ci <- mu * total.vol + (beta * d) %*% m.vol
    }
    ci
}


hawkes.cond.int <- function(params, eval.pts, pts = NULL, data = NULL, TT = NULL) {
    mu <- params[1]
    C <- params[2]
    ak <- params[-(1:2)]
    K <- length(ak)
    
    if(K < 1)
    stop("K must be >= 1")
    if(is.null(TT)) {
        S <- sapply(as.vector(eval.pts), function(x, times, ak, C) {
            if(is.null(times))
            return(0)
            use <- times < x
            if(!is.na(use) && any(use)) {
                d <- x - times[use]
                k <- 0:(length(ak)-1)
                lxk <- outer(log(d), k) + (-C * d)
                sum(exp(lxk) %*% ak)
            }
            else 0
        }, times = as.vector(pts), ak = ak, C = C)
        ci <- mu + S
    }
    else {
        Rfunc <- function(x, L, c) {
            k <- 1:L
            g <- gamma(k) / c^(k)
            
            ## Evaluate the (incomplete) Gamma function for all x and k
            o <- outer(x, k, pgamma, scale = 1/c)
            r <- o * rep(g, rep(nrow(o), ncol(o)))
            ## r <- t(t(o) * g)
            r
        }
        times <- as.vector(pts)
        ci <- mu*(TT[2,1]-TT[1,1])
        S <- double(2)
        
        for(i in 1:2) {
            use <- times < TT[i,1]
            
            if(any(use)) {
                r <- Rfunc(TT[i,1] - times[use], K, C
                )
                S[i] <- sum(r %*% ak)
            }
        }
        ci <- ci + (S[2] - S[1])
    }
    ci
}


gradient.hawkes.cond.int <- function(params, eval.pts, pts = NULL, data = NULL, TT = NULL) {
    mu <- params[1]
    C <- params[2]
    g <- params[3]
    ak <- params[-(1:3)]
    K <- length(ak)
    
    if(K < 1)
    stop("K must be >= 1")
    
    if(is.null(TT)) {
        S <- sapply(as.vector(eval.pts), function(x, times, ak, C) {
            if(is.null(times))
            return(0)
            use <- times < x
            if(!is.na(use) && any(use)) {
                d <- x - times[use]
                k <- 0:(length(ak)-1)
                lxk <- outer(log(d), k) + (-C * d)
                sum(exp(lxk) %*% ak %*% g)
            }
            else 0
        }, times = as.vector(pts), ak = ak, C = C)
        ci <- mu + S
    }
    else {
        Rfunc <- function(x, L, c) {
            k <- 1:L
            g <- gamma(k) / c^(k)
            
            ## Evaluate the (incomplete) Gamma function for all x and k
            o <- outer(x, k, pgamma, scale = 1/c)
            r <- o * rep(g, rep(nrow(o), ncol(o)))
            ## r <- t(t(o) * g)
            r
        }
        times <- as.vector(pts)
        ci <- mu*(TT[2,1]-TT[1,1])
        S <- double(2)
        
        for(i in 1:2) {
            use <- times < TT[i,1]
            
            if(any(use)) {
                r <- Rfunc(TT[i,1] - times[use], K, C
                )
                S[i] <- sum(r %*% ak %*% g)
            }
        }
        ci <- ci + (S[2] - S[1])
    }
    ci
}


hPois.cond.int <- function(params, eval.pts, pts = NULL, data = NULL, TT = NULL) {
    mu <- params[1]
    
    if(is.null(TT))
    rep(mu, nrow(eval.pts))
    else {
        vol <- prod(apply(TT, 2, diff))
        mu * vol
    }
}

PL.hawkes.cond.int <- function(params, eval.pts, pts = NA, data = NULL, TT = NULL) {
    # params <- c(mu = 0.2, C = 1, a = 0.1) # lambda's parameter set
    
    mu <- params[1]
    k <- params[2]
    c <- params[3]
    theta <- params[4]
    
    K <- length(c)
    
    if(K < 1)
    stop("K must be >= 1")
    
    if(is.null(TT)) {   # TT는 NULL
        S <- sapply(as.vector(eval.pts), function(x, times, ak, C){
            use <- times < x
            
            if(!is.na(use) && any(use)) {
                d <- x - times[use] # d = t - t_i
                theta          # c(mu = 0.2, C = 1, a = 0.1)
                sum(k*(1/(d + c)^(1+theta)))
            }
            else 0
        }, times = as.vector(pts), ak = ak, C = C)
        ci <- mu + S
    }else {   # (!is.null(TT))
        times <- as.vector(pts)
        ci <- mu*(TT[2,1]-TT[1,1])
        S <- double(2)
        
        for(i in 1:2) {
            use <- times < TT[i,1]
            
            if(any(use)) {
                d <- as.vector(TT[i,1] - times[use])
                S[i] <- sum(k*(1/(d + c)^(1+theta)))
                S[i]
            }
        }
        ci <- ci + (S[2] - S[1])
    }
    ci
}

