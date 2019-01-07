source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
metric.ind = 29
query.list = c(
  # "THGAL4 x JU30"
  # "CS"
  # "SUN1"
  # "SUN2"
  # "SUN3"
  # "CS x JU30"
  # "MB009B x JU30"
  # "MB131B x JU30"
  # "MB419B x JU30"
  # "MB607B x JU30"
  # "R60D05 x JU30"
  # "JG17 x JU30"
  # "R60D05 x PKCi"
  # "JG17 x PKCi"
  # "CS x PKCi"
  # "MB009B x PKCi"
  # "MB131B x PKCi"
  # "MB419B x PKCi"
  # "MB607B x PKCi"
  # "R60D05 x DopR1-IR",
  # "JG17 x DopR1-IR",
  # "MB009B x DopR1-IR",
  # "MB131B x DopR1-IR",
  # "MB419B x DopR1-IR",
  # "MB607B x DopR1-IR"
)

input.y.df = data.frame()

query.fly = get_query_info(query.list[1])[[1]]
query.experimenter = get_query_info(query.list[1])[[2]]
input.y.df.pre = initial_condition(metric.ind, query.list[1], query.fly=query.fly, query.experimenter=query.experimenter)
colnames(input.y.df.pre) <- c("Value", "Genotype_Sessions")
dunn.test(x = input.y.df.pre$Value, g = input.y.df.pre$Genotype_Sessions, method = c("bonferroni"))
