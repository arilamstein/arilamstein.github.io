library(Quandl)
library(choroplethr)
library(plyr)
library(Hmisc)
library(ggplot2)

# FRED Housing Indices from Quandl
# http://research.stlouisfed.org/fred2/categories/32261
states <- state.abb
state_dfs <- list()
for (i in 1:length(state.abb))
{
  state = state.abb[i]
  print(state)
  q_code = paste0('FRED/', state, 'STHPI')
  state_dfs[[i]] = Quandl(q_code)
  state_dfs[[i]]$state = state
}

df = data.frame(Date=as.Date(character()), Value=numeric(), state=character())
for (i in 1:length(state_dfs)) {
  df = rbind(df, state_dfs[[i]])
}

# each frame needs to be a map of the us, in order
choropleths = list()
dates = sort(unique(df$Date))

for (i in 1:length(dates))
{
  map = df[df$Date == dates[i], ]
  map = rename(map, replace=c("state"="region", "Value"="value"))
  # these cuts cover all values of the dataset
  map$value = cut2(map$value, cuts=c(0, 100, 200, 300, 400, 500, 600, 700, 800))
  choropleths[[i]] = choroplethr(map, "state", 9, title=dates[i])
}

# animation
choroplethr_animate(choropleths)

# this shows the data as a boxplot, which is also interesting
ggplot(df, aes(factor(year(Date)), Value)) + 
  geom_boxplot() + 
  ggtitle("Case Shiller All State Values, by Year") + labs(x="Year", y="Value")
