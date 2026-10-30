# Token-centred systems augmented with four day-ahead electricity markets, and
# the same 25-asset system centred on the gold-backed tokens PAXG and XAUT.
# Reports the token's share of forecast-error variance by source block,
# averaged over 30 evenly spaced dates (100 posterior draws), as in robust_irfa.jl.
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions, Random
include("DynamicNets_functions.jl")

const CUT1 = 5; const CUT2 = 20; const LAG = 1; const HBW = 8

# column order written by 18_electricity_gold.R: token, 24 non-token assets, (electricity)
const CRYPTO = 2:8                                   # BTC ETH SOL AVAX DPI IDEX SPTOP20
const ENFUT  = [9, 17, 18, 19]                       # EUA B CL HH
const GOLD   = [20]                                  # GC
const OTHERMET = [21]                                # HG
const EQUITY = [10, 11, 12, 13, 14, 15, 16, 22]      # NEE ENPH PBW PBD FENY QCLN ICLN LIT
const GREEN  = 23:25                                 # ESGD ESGU BGRN
const ELEC   = 26:29                                 # DE-LU SI IT-North AU-NSW

readdata(s) = Matrix{Float64}(CSV.read("Data/data_$(s).txt", DataFrame, header=false))

function setup(data)
    Tfull, N = size(data); K = N * LAG + 1
    X = zeros(Float64, Tfull - LAG, K - 1)
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

function shares(S; npts=30, Nsim=100, seed=2026)
    pts = unique(round.(Int, range(10, S.T - 10, length=npts)))
    acc = zeros(S.N)
    for (p, it) in enumerate(pts)
        Random.seed!(seed + p)
        M = mean([draw(S, it, i)[1] for i in 1:Nsim])
        acc .+= M[1, :] ./ sum(M[1, :])
    end
    acc ./ length(pts)
end

open("Output/jcm_blocks.csv", "w") do io
    println(io, "system,own,crypto,energy_futures,gold,other_metals,equity,green,electricity,crypto_per_asset,energy_futures_per_asset,electricity_per_asset,gold_per_asset")
    for s in ("PWR_elec", "SNC_elec", "EWT_elec", "PAXG", "XAUT")
        S = setup(readdata(s))
        t = @elapsed r = shares(S)
        el = S.N >= 29 ? sum(r[ELEC]) : NaN
        vals = (100r[1], 100sum(r[CRYPTO]), 100sum(r[ENFUT]), 100sum(r[GOLD]), 100sum(r[OTHERMET]),
                100sum(r[EQUITY]), 100sum(r[GREEN]), 100el,
                100sum(r[CRYPTO]) / length(CRYPTO), 100sum(r[ENFUT]) / length(ENFUT),
                100el / length(ELEC), 100sum(r[GOLD]))
        println(io, s, ",", join(round.(collect(vals), digits=2), ","))
        flush(io)
        println(">>> $s done in $(round(t, digits=1))s")
    end
end
println("DONE_JCM")
