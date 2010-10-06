taustar <-
function (resid, p, conf = 0.95) 
{
    n = length(resid)
    zc = qnorm((1 + conf)/2)
    c1 = (n/2) - ((sqrt(n) * zc)/2) - 0.5
    ic1 = floor(c1)
    if (ic1 < 0) {
        ic1 = 0
    }
    z = sort(resid)
    l = z[ic1 + 1]
    u = z[n - ic1]
    df = sqrt(n)/sqrt(n - p - 1)
    taustar = df * ((sqrt(n) * (u - l))/(2 * zc))
    taustar
}
