# Visualize US Median Household Income, per-county, as an animated choropleth map using the 
# choroplethr package. The visualization is 100 frames: the first frame highlights the top 
# 1% of counties, the second the top 2%, an so on.
# See the choroplethr wiki for details: https://github.com/trulia/choroplethr/wiki
 
library(choroplethr)
library(Hmisc)
library(maps)
library(ggplot2)
 
# Todo: restrict df_income to only include the counties on the map
# i.e. remove counties in AK, HI and PR if present
 
# B19013 = MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2012 INFLATION-ADJUSTED DOLLARS) 
# See http://factfinder2.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=dataset&id=dataset.en.ACS_12_5YR
df_income  = get_acs_df("B19013", "county", 2012, 5) 
min_income = min(df_income$value)
max_income = max(df_income$value)
 
choropleths = list()
for (i in 1:99)
{
  title    = paste0("Top ", i, "% of Counties\nHousehold Income, 2012")
  df       = df_income
  cutoff   = quantile(df_income$value, 1 - (i/100))
  df$value = cut2(df$value, cuts=c(min_income, cutoff, max_income))
  
  # Use the internal choroplethr function format_levels to add commas to the scale 
  levels(df$value) = sapply(levels(df$value), choroplethr:::format_levels)
  
  if (i < 99)
  {
    choropleths[[i]] = choroplethr(df, "county", title=title, scaleName="Median Household Income")  + 
      scale_fill_manual(values=c("grey", "blue"), name="Median Household Income")
  } else {
    choropleths[[i]] = choroplethr(df, "county", title=title, scaleName="Median Household Income")  + 
      scale_fill_manual(values="blue", name="Median Household Income")
  }
}
 
choroplethr_animate(choropleths)