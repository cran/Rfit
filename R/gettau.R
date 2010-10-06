gettau <-
function (ehat, p, phi, phidu , param = c(0), 
    delta = 0.8, hparm = 2) 
{
    temph = hstarready2(ehat, phi, phidu, param)
    abdord = temph$absdifford
    wtord = temph$wtsord
    const = temph$cn
    n = length(ehat)
    templ = looptau(delta, abdord, wtord, const, n, param)
    tn = templ$quan/sqrt(n)
    pn = hstar(abdord, wtord, const, n, tn)
    drivetau = ((phi(1, param) - phi(0, param)) * pn)/(2 * tn)
    drivetau = sqrt(n/(n - p)) * (1/drivetau)
    w = rep(0, n)
    stan = (ehat - median(ehat))/mad(ehat)
    w[abs(stan) < hparm] = 1
    hubcor = sum(w)/n
    if (hubcor < 1e-06) {
        hubcor = 1e-06
    }
    fincor = 1 + (((p)/n) * ((1 - hubcor)/hubcor))
    drivetau = fincor * drivetau
    drivetau
}
