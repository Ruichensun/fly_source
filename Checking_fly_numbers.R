##This snippet is to check how many flies have been tested in each types of genotypes

type_of_mutants = length(unique(fly.info$Genotype))

number_of_mutants = c()
names_of_mutants = unique(fly.info$Genotype)

for (i in 1:type_of_mutants){
  number_of_mutants[i] = dim(fly.info[fly.info$Genotype==names_of_mutants[i],])[1]
}

mutant_info = data.frame(names_of_mutants,number_of_mutants)

colnames(mutant_info) = c("Mutant Genotype", "Number of Flies")

write.csv(mutant_info, file = "Mutant_headcount_020918.csv", row.names = FALSE)
