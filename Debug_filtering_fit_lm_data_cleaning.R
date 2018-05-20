ind = fly.info$Genotype == "WT"

fms <- fly.info$Fly.moving.speed[ind]

rank_fms = rank(fms)
ind.filter =  rank_fms <= length(fms) * 0.95 &
  rank_fms >= length(fms) * 0.2


print(quantile(fms,c(0.05,0.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,1)))


fms_filtered = fms[ind.filter]



print(range(fms_filtered))