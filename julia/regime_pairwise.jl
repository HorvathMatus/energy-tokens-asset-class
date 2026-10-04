# Per-regime token-to-block spillovers (#29 completion).
# Samples DynNet_table at evenly-spaced time points inside each sub-period and
# averages the pairwise FEVD, then reads off the token's FROM-block shares.
using Pkg; Pkg.activate(".")
using CSV, DataFrames, LinearAlgebra, Statistics, Distributions
include("DynamicNets_functions.jl")

systems = ["PWR","SNC","EWT"]
regimes = [("turmoil",269,391), ("crisis",392,518), ("recovery",519,1281)]
crypto = 2:8; energy = 9:22; green = 23:25   # asset 1 = token
npts = 6

open("Output/regime_pairwise.csv","w") do io
  println(io,"system,regime,own,from_crypto,from_energy,from_green")
  for s in systems
    data = Matrix{Float64}(CSV.read("Data/data_$(s).txt", DataFrame, header=false))
    for (rg,lo,hi) in regimes
      pts = unique(round.(Int, range(lo, hi, length=npts)))
      acc = zeros(25,25); k = 0
      for t in pts
        tt, = DynNet_table(data, t, 5, 20, 1, 8, 100, false)
        acc .+= Matrix(tt); k += 1
      end
      M = acc ./ k
      row = M[1,:] ./ sum(M[1,:])     # token's variance decomposition (row 1)
      println(io, join((s, rg,
        round(100*row[1],digits=1),
        round(100*sum(row[crypto]),digits=1),
        round(100*sum(row[energy]),digits=1),
        round(100*sum(row[green]),digits=1)), ","))
      flush(io)
      println(">>> done $s $rg  (", k, " points)")
    end
  end
end
println("DONE_REGIME_PAIRWISE")
