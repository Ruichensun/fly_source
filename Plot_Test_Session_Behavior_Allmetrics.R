#This script is for plotting all metrics relevant after fit_lm_data_cleaning.R

#WT flies
query.genotype <- c("WT","CS")
query.fly = fly.info.end[((fly.info.end$Genotype == "WT") |
                                (fly.info.end$Genotype == "CS")) &
                               (fly.info.end$Experimenter!="SW"), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "WT") |
                                         (fly.info.end$Genotype == "CS")) &
                                        (fly.info.end$Experimenter!="SW"), ]$Experimenter

#MB009B x DopR1-IR flies

query.genotype <- c("MB009B x DopR1-IR")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB009B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB009B x DopR1-IR")), ]$Experimenter



#MB419B x DopR1-IR flies

query.genotype <- c("MB419B x DopR1-IR")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB419B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB419B x DopR1-IR")), ]$Experimenter



#MB607B x DopR1-IR flies

query.genotype <- c("MB607B x DopR1-IR")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB607B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB607B x DopR1-IR")), ]$Experimenter



#SUN1 flies
query.genotype <- c("SUN1")
query.fly = fly.info.end[((fly.info.end$Genotype == "SUN1")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "SUN1")), ]$Experimenter


#SUN2 flies
query.genotype <- c("SUN2")
query.fly = fly.info.end[((fly.info.end$Genotype == "SUN2")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "SUN2")), ]$Experimenter


#SUN3 flies
query.genotype <- c("SUN3")
query.fly = fly.info.end[((fly.info.end$Genotype == "SUN3")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "SUN3")), ]$Experimenter


#"MB607B x JU30" flies
query.genotype <- c("MB607B x JU30")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB607B x JU30")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB607B x JU30")), ]$Experimenter


#"MB009B x JU30" flies
query.genotype <- c("MB009B x JU30")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB009B x JU30")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB009B x JU30")), ]$Experimenter


#"MB131B x JU30" flies
query.genotype <- c("MB131B x JU30")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB131B x JU30")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB131B x JU30")), ]$Experimenter


#"MB419B x JU30" flies
query.genotype <- c("MB419B x JU30")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB419B x JU30")), ]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB419B x JU30")), ]$Experimenter

#R60D05 x JU30 flies
query.genotype <- c("R60D05 x JU30")
query.fly = fly.info.end[(fly.info.end$Genotype == "R60D05 x JU30"),]$Fly
query.experimenter = fly.info.end[(fly.info.end$Genotype == "R60D05 x JU30"),]$Experimenter


#JG17 x JU30
query.genotype <- c("JG17 x JU30")
query.fly = fly.info.end[((fly.info.end$Genotype == "JG17 x JU30")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "JG17 x JU30")),]$Experimenter


#CS x JU30
query.genotype <- c("CS x JU30")
query.fly = fly.info.end[((fly.info.end$Genotype == "CS x JU30")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "CS x JU30")),]$Experimenter

#R60D05 x PKCi 
query.genotype <- c("R60D05 x PKCi", "R60D05 x PKCI")
query.fly = fly.info.end[((fly.info.end$Genotype == "R60D05 x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "R60D05 x PKCi")),]$Experimenter


#JG17 x PKCi
query.genotype <- c("JG17 x PKCi")
query.fly = fly.info.end[((fly.info.end$Genotype == "JG17 x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "JG17 x PKCi")),]$Experimenter


#CS x PKCi
query.genotype <- c("CS x PKCi")
query.fly = fly.info.end[((fly.info.end$Genotype == "CS x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "CS x PKCi")),]$Experimenter


#MB009B x PKCi 
query.genotype <- c("MB009B x PKCi")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB009B x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB009B x PKCi")),]$Experimenter

#MB131B x PKCi
query.genotype <- c("MB131B x PKCi")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB131B x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB131B x PKCi")),]$Experimenter


#MB419B x PKCi
query.genotype <- c("MB419B x PKCi")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB419B x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB419B x PKCi")),]$Experimenter


#MB607B x PKCi
query.genotype <- c("MB607B x PKCi")
query.fly = fly.info.end[((fly.info.end$Genotype == "MB607B x PKCi")),]$Fly
query.experimenter = fly.info.end[((fly.info.end$Genotype == "MB607B x PKCi")),]$Experimenter

