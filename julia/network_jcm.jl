# Full time-varying frequency connectedness for the JCM token-centred systems:
# token, 24 other assets, and four electricity markets (29 assets), with the
# exchange-traded assets aligned to the token's day (data_<SYS>_elec_shift.txt).
# One pass over all dates gives both outputs of the original pipeline:
#   Chorizon / CIhorizon1 / CIhorizon2 (posterior median and 95% bands of all
#   DynNet measures, as in DynNet) and avg_tab_{total,long,medium,short}
#   (sample-average pairwise tables, as in the DynNet_table loop of 02_julia.ipynb).
# Usage: julia network_jcm.jl PWR   (or SNC, EWT)
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions, Random
include("DynamicNets_functions.jl")

const CUT1 = 5; const CUT2 = 20; const LAG = 1; const HBW = 8; const NSIM = 100
tok = ARGS[1]
sys = "$(tok)_elec_shift"
data = Matrix{Float64}(CSV.read("Data/data_$(sys).txt", DataFrame, header=false))

Tfull, N = size(data)
X = zeros(Float64, Tfull - LAG, N * LAG)
for i in 1:LAG
    temp = lag0(data, i); X[:, (1 + N*(i-1)):(i*N)] = temp[(1+LAG):Tfull, :]
end
y = data[(1+LAG):Tfull, :]; T = Tfull - LAG
X = [ones(Float64, T, 1) X]
SI, PI, a, RI = MinnNWprior(data, T, N, LAG, 0.05)
w1 = convert.(Float64, normker(T, HBW)); pp = convert.(Float64, inv(PI))

# one posterior draw: the DynNet measure vector and the four band tables
function f_both(it)
    ba, bs, bg, BB = oneTimePrior(it, w1, pp, X, y, SI, PI, a, RI)
    B0, A0 = getCoefsStable(ba, bg, bs, BB, N, LAG)
    wold = get_GIRF_fast(B0, A0, 1, N, LAG, 100)
    m = get_dynnet(wold, T, A0, false, CUT1, CUT2)
    v = [[m[1], m[2], m[3], m[4], m[5], m[6], m[7]]; vcat(m[8:end]...)]
    tabs = get_dynnet_table(wold, T, A0, false, CUT1, CUT2)
    return v, tabs
end

nm = 7 + (2*4*2 + 4) * N
xmean = zeros(nm, T); xci1 = zeros(nm, T); xci2 = zeros(nm, T)
acc = [zeros(N, N) for _ in 1:4]
Random.seed!(2026)
t0 = time()
Tmax = length(ARGS) > 1 ? parse(Int, ARGS[2]) : T   # second argument: short test run
for it in 1:Tmax
    res = [f_both(it) for _ in 1:NSIM]
    out = reduce(hcat, first.(res))
    for k in 1:nm
        r = filter(!isnan, out[k, :])   # a few draws can return NaN, as in the table averages
        isempty(r) && (xmean[k, it] = xci1[k, it] = xci2[k, it] = NaN; continue)
        xmean[k, it] = quantile(r, 0.5); xci1[k, it] = quantile(r, 0.025); xci2[k, it] = quantile(r, 0.975)
    end
    for b in 1:4
        acc[b] .+= [mean(filter(!isnan, [res[d][2][b][i, j] for d in 1:NSIM])) for i in 1:N, j in 1:N]
    end
    if it % 50 == 0
        println(">>> $tok $it/$T  $(round((time() - t0) / 60, digits=1)) min"); flush(stdout)
    end
end

tag = Tmax < T ? "$(tok)_elecs_test" : "$(tok)_elecs"
CSV.write("Output/Chorizon_$(tag).csv", DataFrame(xmean[:, 1:Tmax], :auto))
CSV.write("Output/CIhorizon1_$(tag).csv", DataFrame(xci1[:, 1:Tmax], :auto))
CSV.write("Output/CIhorizon2_$(tag).csv", DataFrame(xci2[:, 1:Tmax], :auto))
for (b, nmb) in enumerate(("total", "long", "medium", "short"))
    CSV.write("Output/avg_tab_$(nmb)_$(tag).csv", DataFrame(acc[b] ./ Tmax, :auto))
end
println("DONE_NETWORK_$tok")
