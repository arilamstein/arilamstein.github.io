# make an animated choropleth showing the results of all US presidential elections

choropleths = list()
for (i in 2:ncol(df_president_ts)) {
  df           = df_president_ts[, c(1, i)]
  colnames(df) = c("region", "value")
  title        = paste0("Presidential Election Results: ", colnames(df_president_ts)[i])
  choropleths[[i-1]] = choroplethr(df, "state", title=title)
}
choroplethr_animate(choropleths)
