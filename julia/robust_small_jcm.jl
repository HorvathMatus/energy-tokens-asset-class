# Small token-centred systems with one common information set (JCM referee check).
# Every system contains the token, two crypto factors (BTC, ETH), one clean-energy
# equity fund (ICLN), Brent, Henry Hub gas, the EU carbon allowance, gold, copper,
# and the four electricity markets (13 assets), with the exchange-traded assets
# aligned to the token's day. The same system is estimated for the three liquid
# energy tokens and for the two gold-backed tokens, so the shares do not depend on
# how many crypto series enter the system.
# Reports the token's forecast-error-variance shares by source, for the total
# table and within each frequency band (band table rows normalised to one), and
# the weight of each band in the token's forecast-error variance.
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions, Random
include("DynamicNets_functions.jl")

const CUT1 = 5; const CUT2 = 20; const LAG = 1; const HBW = 8

readdata(s) = Matrix{Float64}(CSV.read("Data/data_$(s).txt", DataFrame, header=false))
# columns of the 29- and 25-asset files: 1 token, 2 BTC, 3 ETH, 9 EUA, 16 ICLN, 17 B, 19 HH, 20 GC, 21 HG, 26:29 electricity
const PICK = [1, 2, 3, 16, 17, 19, 9, 20, 21]
const NAMES = ["own", "BTC", "ETH", "ICLN", "B", "HH", "EUA", "GC", "HG", "EL_DE", "EL_SI", "EL_ITN", "EL_AU"]
elec = readdata("PWR_elec_shift")[:, 26:29]
small(s) = s in ("PAXG", "XAUT") ? hcat(readdata("$(s)_shift")[:, PICK], elec) : readdata("$(s)_elec_shift")[:, [PICK; 26:29]]

function setup(data)
    Tfull, N = size(data)
    X = zeros(Float64, Tfull - LAG, N * LAG)
    for i in 1:LAG
        temp = lag0(data, i); X[:, (1 + N*(i-1)):(i*N)] = temp[(1+LAG):Tfull, :]
    end
    y = data[(1+LAG):Tfull, :]; T = Tfull - LAG
    X = [ones(Float64, T, 1) X]
    SI, PI, a, RI = MinnNWprior(data, T, N, LAG, 0.05)
    (T=T, N=N, X=X, y=y, SI=SI, PI=PI, a=a, RI=RI,
     w=convert.(Float64, normker(T, HBW)), pp=convert.(Float64, inv(PI)))
end
draw(S, it, i) = f_all_table(it, i, CUT1, CUT2, S.T, S.N, LAG, S.w, S.pp, S.X, S.y, S.SI, S.PI, S.a, S.RI, false)

function rows(S; npts=30, Nsim=100, seed=2026)
    pts = unique(round.(Int, range(10, S.T - 10, length=npts)))
    acc = [zeros(S.N) for _ in 1:4]; wt = zeros(4)
    for (p, it) in enumerate(pts)
        Random.seed!(seed + p)
        out = [draw(S, it, i) for i in 1:Nsim]
        out = filter(o -> !any(isnan, o[1]), out)
        tabs = [mean([o[b] for o in out]) for b in 1:4]
        tot = sum(tabs[1][1, :])
        for b in 1:4
            r = tabs[b][1, :]
            acc[b] .+= r ./ sum(r)
            wt[b] += sum(r) / tot
        end
    end
    [a ./ length(pts) for a in acc], wt ./ length(pts)
end

open("Output/jcm_small_systems.csv", "w") do io
    println(io, "system,band,band_weight," * join(NAMES, ","))
    for s in ("PWR", "SNC", "EWT", "PAXG", "XAUT")
        S = setup(small(s))
        t = @elapsed (acc, wt) = rows(S)
        for (b, nm) in enumerate(("total", "long", "medium", "short"))
            println(io, "$s,$nm,$(round(100wt[b], digits=2))," * join(round.(100 .* acc[b], digits=2), ","))
        end
        flush(io)
        println(">>> $s done in $(round(t, digits=1))s")
    end
end
println("DONE_SMALL")
