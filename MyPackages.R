write.csv(
  data.frame(installed.packages()),
  file.path("reports", "packages.csv"),
  row.names = F)