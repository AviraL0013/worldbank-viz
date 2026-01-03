# World Bank Data Visualization
# Chapter 8 Exercises

library(animint2)
library(data.table)

data(WorldBank)
WorldBank$Region <- sub(" (all income levels)", "", WorldBank$region, fixed=TRUE)

not.na <- data.table(WorldBank)[!(is.na(life.expectancy) | is.na(fertility.rate))]
not.na[is.na(population), population := 1700000]

# Helper functions for faceting
FACETS <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}

TS.RIGHT <- function(df) FACETS(df, "Years", "Life expectancy")
SCATTER <- function(df) FACETS(df, "Fertility rate", "Life expectancy")
TS.ABOVE <- function(df) FACETS(df, "Fertility rate", "Years")

years <- unique(not.na[, .(year)])

# Time series plot for life expectancy
ts.right <- ggplot()+
  geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2),
    clickSelects="year",
    data=TS.RIGHT(years), alpha=1/2)+
  geom_line(aes(year, life.expectancy, group=country, colour=Region),
    clickSelects="country",
    data=TS.RIGHT(not.na), size=4, alpha=3/5)

ts.facet <- ts.right+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(side ~ top, scales="free")+
  xlab("")+
  ylab("")

# Add scatterplot
ts.scatter <- ts.facet+
  theme_animint(width=600)+
  geom_point(aes(fertility.rate, life.expectancy,
    colour=Region, size=population, key=country),
    clickSelects="country",
    showSelected="year",
    data=SCATTER(not.na))+
  scale_size_animint(pixel.range=c(2, 20), breaks=10^(9:5))

# Add fertility time series
scatter.both <- ts.scatter+
  geom_widerect(aes(ymin=year-1/2, ymax=year+1/2),
    clickSelects="year",
    data=TS.ABOVE(years), alpha=1/2)+
  geom_path(aes(fertility.rate, year, group=country, colour=Region),
    clickSelects="country",
    data=TS.ABOVE(not.na), size=4, alpha=3/5)

# Exercise 1: Points on time series
scatter.ex1 <- scatter.both +
  geom_point(aes(year, life.expectancy,
    colour=Region, size=population, key=country),
    showSelected="country",
    clickSelects="country",
    data=TS.RIGHT(not.na))

# Exercise 2: Text labels on time series
scatter.ex2 <- scatter.ex1 +
  geom_text(aes(year, life.expectancy, label=country, key=country),
    showSelected="country",
    clickSelects="country",
    data=TS.RIGHT(not.na),
    hjust=1, vjust=0, size=3)

# Exercise 3: Year label on scatter
scatter.ex3 <- scatter.ex2 +
  geom_text(aes(7, 80, label=paste("Year:", year), key=year),
    showSelected="year",
    data=SCATTER(years),
    size=6, hjust=1, vjust=1)

# Exercise 4: Country labels on scatter
scatter.final <- scatter.ex3 +
  geom_text(aes(fertility.rate, life.expectancy, label=country, key=country),
    showSelected="country",
    clickSelects="country",
    data=SCATTER(not.na),
    hjust=1, vjust=0, size=3)

# Create final visualization
viz.complete <- animint(
  title="World Bank Data Visualization",
  source="https://github.com/AviraL0013/worldbank-viz",
  scatterComplete=scatter.final+theme_animint(width=1000, height=800),
  duration=list(year=1000),
  time=list(variable="year", ms=3000),
  first=list(year=1975, country=c("United States", "China", "India")),
  selector.types=list(country="multiple"))

animint2dir(viz.complete, "WorldBank-complete")
