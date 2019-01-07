#This script is for plotting all metrics relevant after fit_lm_data_cleaning.R

fly.info.include = fly.info[ind.include, ]
#WT flies
query.genotype <- c("WT","CS")
query.fly = fly.info.include[((fly.info.include$Genotype == "WT") |
                                (fly.info.include$Genotype == "CS")) &
                               (fly.info.include$experimenter!="SW"), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "WT") |
                                         (fly.info.include$Genotype == "CS")) &
                                        (fly.info.include$experimenter!="SW"), ]$experimenter

#MB009B x DopR1-IR flies

query.genotype <- c("MB009B x DopR1-IR")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB009B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB009B x DopR1-IR")), ]$experimenter



#MB419B x DopR1-IR flies

query.genotype <- c("MB419B x DopR1-IR")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB419B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB419B x DopR1-IR")), ]$experimenter



#MB607B x DopR1-IR flies

query.genotype <- c("MB607B x DopR1-IR")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB607B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB607B x DopR1-IR")), ]$experimenter



#SUN1 flies
query.genotype <- c("SUN1")
query.fly = fly.info.include[((fly.info.include$Genotype == "SUN1")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN1")), ]$experimenter


#SUN2 flies
query.genotype <- c("SUN2")
query.fly = fly.info.include[((fly.info.include$Genotype == "SUN2")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN2")), ]$experimenter


#SUN3 flies
query.genotype <- c("SUN3")
query.fly = fly.info.include[((fly.info.include$Genotype == "SUN3")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN3")), ]$experimenter


#"MB607B x JU30" flies
query.genotype <- c("MB607B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB607B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB607B x JU30")), ]$experimenter


#"MB009B x JU30" flies
query.genotype <- c("MB009B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB009B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB009B x JU30")), ]$experimenter


#"MB131B x JU30" flies
query.genotype <- c("MB131B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB131B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB131B x JU30")), ]$experimenter


#"MB419B x JU30" flies
query.genotype <- c("MB419B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB419B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB419B x JU30")), ]$experimenter

#R60D05 x JU30 flies
query.genotype <- c("R60D05 x JU30")
query.fly = fly.info.include[(fly.info.include$Genotype == "R60D05 x JU30"),]$Fly
query.experimenter = fly.info.include[(fly.info.include$Genotype == "R60D05 x JU30"),]$experimenter


#JG17 x JU30
query.genotype <- c("JG17 x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),]$experimenter


#CS x JU30
query.genotype <- c("CS x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "CS x JU30")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "CS x JU30")),]$experimenter

#R60D05 x PKCi 
query.genotype <- c("R60D05 x PKCi", "R60D05 x PKCI")
query.fly = fly.info.include[((fly.info.include$Genotype == "R60D05 x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "R60D05 x PKCi")),]$experimenter


#JG17 x PKCi
query.genotype <- c("JG17 x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "JG17 x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "JG17 x PKCi")),]$experimenter


#CS x PKCi
query.genotype <- c("CS x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "CS x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "CS x PKCi")),]$experimenter


#MB009B x PKCi 
query.genotype <- c("MB009B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB009B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB009B x PKCi")),]$experimenter

#MB131B x PKCi
query.genotype <- c("MB131B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB131B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB131B x PKCi")),]$experimenter


#MB419B x PKCi
query.genotype <- c("MB419B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB419B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB419B x PKCi")),]$experimenter


#MB607B x PKCi
query.genotype <- c("MB607B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB607B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB607B x PKCi")),]$experimenter

