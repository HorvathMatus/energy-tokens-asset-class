# Robustness of the connectedness decomposition in the JCM baseline system
# (POWR, 24 other assets, four electricity markets, aligned trading sessions):
# alternative VAR lag orders and frequency bands. Total and band connectedness
# are averaged over 100 posterior draws at 30 evenly spaced dates, as in robust_jcm.jl.
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions, Random
include("DynamicNets_functions.jl")

const HBW = 8
data = Matrix{Float64}(CSV.read("Data/data_PWR_elec_shift.txt", DataFrame, header=false))

function setup(data, L)
    Tfull, N = size(data)
    X = zeros(Float64, Tfull - L, N * L)
    for i in 1:L
        temp = lag0(data, i); X[:, (1 + N*(i-1)):(i*N)] = temp[(1+L):Tfull, :]
    end
    y = data[(1+L):Tfull, :]; T = Tfull - L
    X = [ones(Float64, T, 1) X]
    SI, PI, a, RI = MinnNWprior(data, T, N, L, 0.05)
    (T=T, N=N, L=L, X=X, y=y, SI=SI, PI=PI, a=a, RI=RI,
     w=convert.(Float64, normker(T, HBW)), pp=convert.(Float64, inv(PI)))
end

function conn(S, c1, c2; npts=30, Nsim=100, seed=2026)
    pts = unique(round.(Int, range(10, S.T - 10, length=npts)))
    acc = zeros(4); n = 0
    for (p, it) in enumerate(pts)
        Random.seed!(seed + p)
        for i in 1:Nsim
            th, tl, tm, ts = f_all_table(it, i, c1, c2, S.T, S.N, S.L, S.w, S.pp, S.X, S.y, S.SI, S.PI, S.a, S.RI, false)
            b = [100 * (sum(t) - tr(t)) / sum(th) for t in (tl, tm, ts)]
            any(isnan, b) && continue
            acc .+= [sum(b); b]; n += 1
        end
    end
    acc ./ n
end

open("Output/jcm_tvpvar_robust.csv", "w") do io
    println(io, "config,L,cut1,cut2,total,long,medium,short")
    for (tag, L, c1, c2) in (("baseline", 1, 5, 20), ("lag2", 2, 5, 20), ("lag3", 3, 5, 20), ("bandalt", 1, 10, 60))
        t = @elapsed m = conn(setup(data, L), c1, c2)
        println(io, "$tag,$L,$c1,$c2," * join(round.(m, digits=2), ",")); flush(io)
        println(">>> $tag done in $(round(t, digits=1))s")
    end
end
println("DONE_TVPVAR_JCM")
