# check if files are incorrectely labelled

mislabelled = list()

for(ind in 1:nrow(fly.info.end)){
  for(ind.session in 1:length(query.sessions)){
    if (fly.info.end$Genotype[ind]=="WT"){
      input.file <- list.files(path = paste0("data/", fly.info.end$Experimenter[ind], "/CS/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info.end$Fly[ind], "_*"),
                               full.names=F)
    }else if(fly.info.end$Genotype[ind]=="CS"){
      input.file <- list.files(path = paste0("data/", fly.info.end$Experimenter[ind], "/mutants/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info.end$Fly[ind], "_*"),
                               full.names=F)
    }
    if(length(input.file) < 5 ){
      mislabelled = append(input.file, mislabelled)
    }
    }
    
  }

