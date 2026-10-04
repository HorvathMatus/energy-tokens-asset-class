# Robustness of the frequency-connectedness decomposition (#18 lag, #19 bands)
# Re-runs DynNet on the 25-asset POWR system under alternative VAR lag orders
# and alternative frequency-band cutoffs, and reports the time-average of the
# total and band-specific connectedness (rows 1-4 of the DynNet output).
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions
include("DynamicNets_functions.jl")

data = Matrix{Float64}(CSV.read("Data/data_PWR.txt", DataFrame, header=false))
println("data size: ", size(data))

# (tag, L, cut1, cut2)
configs = [("baseline", 1, 5, 20),   # paper baseline
           ("lag2",     2, 5, 20),   # #18 lag robustness
           ("lag3",     3, 5, 20),   # #18 lag robustness
           ("bandalt",  1, 10, 60)]  # #19 alternative frequency bands

open("Output/robust_summary.csv", "w") do io
    println(io, "config,L,cut1,cut2,total,band1,band2,band3,row5,row6,row7")
    for (tag, L, c1, c2) in configs
        println(">>> CONFIG $tag : L=$L, cut=($c1,$c2)")
        t = @elapsed (C, CI1, CI2) = DynNet(data, c1, c2, L, 8, 100, false)
        m = [mean(C[r, :]) for r in 1:7]
        println("    elapsed=$(round(t,digits=1))s  rowmeans(1:7)=", round.(m, digits=2))
        CSV.write("Output/robust_Chorizon_$(tag).csv", DataFrame(C, :auto))
        println(io, "$tag,$L,$c1,$c2," * join(round.(m, digits=3), ","))
        flush(io)
    end
end
println("DONE_ROBUST")
