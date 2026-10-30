# Sensitivity of the POWR-centred 29-asset system (aligned trading sessions) to the
# choices of the local estimator: the width of the Gaussian kernel (8 trading days
# in the baseline, 16, and 36, which is about the square root of the sample size)
# and the overall Minnesota shrinkage (0.05 in the baseline, 0.025 and 0.10).
# Reports total connectedness and its bands, and the token's forecast-error-variance
# shares from the crypto block, the electricity markets, and the energy futures,
# averaged over 100 posterior draws at 30 evenly spaced dates.
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions, Random
include("DynamicNets_functions.jl")

const CUT1 = 5; const CUT2 = 20; const LAG = 1
const CRYPTO = 2:8; const ENFUT = [9, 17, 18, 19]; const ELEC = 26:29
data = Matrix{Float64}(CSV.read("Data/data_PWR_elec_shift.txt", DataFrame, header=false))

function setup(data, hbw, shrink)
    Tfull, N = size(data)
    X = zeros(Float64, Tfull - LAG, N * LAG)
    for i in 1:LAG
        temp = lag0(data, i); X[:, (1 + N*(i-1)):(i*N)] = temp[(1+LAG):Tfull, :]
    end
    y = data[(1+LAG):Tfull, :]; T = Tfull - LAG
    X = [ones(Float64, T, 1) X]
    SI, PI, a, RI = MinnNWprior(data, T, N, LAG, shrink)
    (T=T, N=N, X=X, y=y, SI=SI, PI=PI, a=a, RI=RI,
     w=convert.(Float64, normker(T, hbw)), pp=convert.(Float64, inv(PI)))
end

function measures(S; npts=30, Nsim=100, seed=2026)
    pts = unique(round.(Int, range(10, S.T - 10, length=npts)))
    acc = zeros(7); n = 0
    for (p, it) in enumerate(pts)
        Random.seed!(seed + p)
        for i in 1:Nsim
            th, tl, tm, ts = f_all_table(it, i, CUT1, CUT2, S.T, S.N, LAG, S.w, S.pp, S.X, S.y, S.SI, S.PI, S.a, S.RI, false)
            b = [100 * (sum(t) - tr(t)) / sum(th) for t in (tl, tm, ts)]
            (any(isnan, b) || any(isnan, th)) && continue
            r = th[1, :] ./ sum(th[1, :])
            acc .+= [sum(b); b; 100sum(r[CRYPTO]); 100sum(r[ELEC]); 100sum(r[ENFUT])]; n += 1
        end
    end
    acc ./ n
end

open("Output/jcm_spec_robust.csv", "w") do io
    println(io, "config,bandwidth,shrinkage,total,long,medium,short,tok_crypto,tok_elec,tok_enfut")
    for (tag, hbw, sh) in (("baseline", 8, 0.05), ("bw16", 16, 0.05), ("bw36", 36, 0.05),
                           ("shrink025", 8, 0.025), ("shrink10", 8, 0.10))
        t = @elapsed m = measures(setup(data, hbw, sh))
        println(io, "$tag,$hbw,$sh," * join(round.(m, digits=2), ",")); flush(io)
        println(">>> $tag done in $(round(t, digits=1))s")
    end
end
println("DONE_SPEC")
