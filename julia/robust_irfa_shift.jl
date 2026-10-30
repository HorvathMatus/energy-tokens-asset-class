# Robustness of the token-centred 25-asset networks (JCM version, exchange-traded assets aligned with the token day).
#   (A) All six token systems, token FROM-block shares (crypto / energy / green)
#       and token net directional spillover, total and transitory/persistent bands.
#   (B) Same measures with the token return winsorised at 1%/99%, so that no
#       single extreme (possibly illiquidity-driven) day can dominate.
#   (C) Posterior-draw stability: Nsim = 100 vs 500 vs 1000, per-draw quantiles.
# Pairwise tables are evaluated at evenly spaced time points with the same
# posterior machinery as DynNet_table (Barunik & Ellington 2024).
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions, Random
include("DynamicNets_functions.jl")

const CRY = 2:8; const ENE = 9:22; const GRN = 23:25
const CUT1 = 5; const CUT2 = 20; const LAG = 1; const HBW = 8

readdata(s) = shiftx(Matrix{Float64}(CSV.read("Data/data_$(s).txt", DataFrame, header=false)))
# exchange-traded assets (columns 8 to 25: SPTOP20 ... BGRN) aligned with the token day
function shiftx(d); d = copy(d); d[2:end, 8:25] = d[1:end-1, 8:25]; d[1, 8:25] .= 0; d[2:end, :]; end

function winsor(data; lo=0.01, hi=0.99)
    d = copy(data); x = d[:, 1]
    ql, qh = quantile(x, lo), quantile(x, hi)
    d[:, 1] = clamp.(x, ql, qh); d
end

# Same pre-processing as DynNet_table, done once per dataset
function setup(data)
    Tfull, N = size(data); K = N * LAG + 1
    X = zeros(Float64, Tfull - LAG, K - 1)
    for i in 1:LAG
        temp = lag0(data, i); X[:, (1 + N*(i-1)):(i*N)] = temp[(1+LAG):Tfull, :]
    end
    y = data[(1+LAG):Tfull, :]; T = Tfull - LAG
    X = [ones(Float64, T, 1) X]
    SI, PI, a, RI = MinnNWprior(data, T, N, LAG, 0.05)
    w = convert.(Float64, normker(T, HBW)); pp = convert.(Float64, inv(PI))
    (T=T, N=N, X=X, y=y, SI=SI, PI=PI, a=a, RI=RI, w=w, pp=pp)
end

draw(S, it, i) = f_all_table(it, i, CUT1, CUT2, S.T, S.N, LAG, S.w, S.pp,
                             S.X, S.y, S.SI, S.PI, S.a, S.RI, false)

# token measures from one table M (rows receive, columns transmit)
function tokmeas(M)
    own = M[1, 1]
    fc, fe, fg = sum(M[1, CRY]), sum(M[1, ENE]), sum(M[1, GRN])
    to = sum(M[:, 1]) - M[1, 1]; from = sum(M[1, :]) - M[1, 1]
    [own, fc, fe, fg, to - from]
end

points(T; n=30) = unique(round.(Int, range(10, T - 10, length=n)))

# band tables: 1 = total, 2 = long (persistent, >20d), 4 = short (transitory, <=5d)
const BANDS = [("total", 1), ("persistent", 2), ("transitory", 4)]

function block_summary(S, pts, Nsim; seed=2026)
    acc = Dict(b => zeros(5) for (b, _) in BANDS); k = 0
    for (p, it) in enumerate(pts)
        Random.seed!(seed + p)
        out = [draw(S, it, i) for i in 1:Nsim]
        for (b, ix) in BANDS
            Mbar = mean([o[ix] for o in out])
            acc[b] .+= tokmeas(Mbar)
        end
        k += 1
    end
    Dict(b => acc[b] ./ k for (b, _) in BANDS)
end

systems = ["PWR", "SNC", "EWT", "WOZX", "WPR", "SLR"]

# ── (A) + (B) ────────────────────────────────────────────────────────────────
open("Output/jcm_irfa_blocks_shift.csv", "w") do io
    println(io, "system,variant,band,own,from_crypto,from_energy,from_green,net")
    for s in systems
        raw = readdata(s)
        variants = s in ("PWR", "SNC", "EWT", "WOZX") ?
                   [("baseline", raw), ("winsor1", winsor(raw))] : [("baseline", raw)]
        for (v, d) in variants
            S = setup(d); pts = points(S.T)
            t = @elapsed res = block_summary(S, pts, 100)
            for (b, _) in BANDS
                m = res[b]
                println(io, join((s, v, b, round.(100 .* m, digits=2)...), ","))
            end
            flush(io)
            println(">>> $s $v done in $(round(t, digits=1))s")
        end
    end
end

# ── (C) posterior-draw stability ─────────────────────────────────────────────
open("Output/jcm_irfa_draws_shift.csv", "w") do io
    println(io, "system,Nsim,measure,median,q025,q975")
    for s in ("PWR", "SNC")
        S = setup(readdata(s)); pts = points(S.T; n=10)
        for Nsim in (100, 500, 1000)
            med = zeros(2); lo = zeros(2); hi = zeros(2)
            t = @elapsed for (p, it) in enumerate(pts)
                Random.seed!(7000 + p)
                vals = [tokmeas(draw(S, it, i)[1]) for i in 1:Nsim]
                fc = [v[2] for v in vals]; nt = [v[5] for v in vals]
                for (j, x) in enumerate((fc, nt))
                    med[j] += quantile(x, 0.5); lo[j] += quantile(x, 0.025); hi[j] += quantile(x, 0.975)
                end
            end
            np = length(pts)
            for (j, nm) in enumerate(("from_crypto", "net"))
                println(io, join((s, Nsim, nm, round(100 * med[j] / np, digits=2),
                                  round(100 * lo[j] / np, digits=2),
                                  round(100 * hi[j] / np, digits=2)), ","))
            end
            flush(io)
            println(">>> draws $s Nsim=$Nsim done in $(round(t, digits=1))s")
        end
    end
end
println("DONE_IRFA_SHIFT")
