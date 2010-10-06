hstarready2 <-
function (ehat, phi , phipr , param = c(0)) 
{
    orde = sort(ehat)
    const = phi(1, param) - phi(0, param)
    n = length(ehat)
    ic = (1:n)/(n + 1)
    vphi = rep(0, n)
    cn = 0
    for (i in 1:n) {
        vphi[i] = phipr(i/(n + 1), param)
        cn = cn + (phipr(i/(n + 1), param))/const
    }
    vad = pairup(orde)
    vwt = pairup(vphi)/const
    vad2 = abs(vad[, 1] - vad[, 2])
    vwt2 = vwt[, 1] + vwt[, 2]
    iord = order(vad2)
    absdifford = vad2[iord]
    wtsord = vwt2[iord]
    list(absdifford = absdifford, wtsord = wtsord, cn = cn)
}
