##This snippet is to check how many flies have been tested in each types of genotypes

# Filename is a string. Example: "Mutant_headcount_110818.csv"

checking_fly_numbers <- function(fly.info.include, filename){

  type_of_mutants = length(unique(fly.info.include$Genotype))
  number_of_mutants = c()
  names_of_mutants = unique(fly.info.include$Genotype)
  
  for (i in 1:type_of_mutants){
    number_of_mutants[i] = dim(fly.info.include[fly.info.include$Genotype==names_of_mutants[i],])[1]
  }
  
  mutant_info = data.frame(names_of_mutants,number_of_mutants)
  
  colnames(mutant_info) = c("Mutant Genotype", "Number of Flies")
  
  write.csv(mutant_info, file = filename , row.names = FALSE)
}

