# What Makes Them Ride? An Analysis of Public Transportation in 9 Major American Cities
## Author: Raza Lamb

This project pulled data from 8 different sources, including the U.S. Bureau of Labor Statistics (BLS), the Federal Reserve Economic Data (FRED), the National Transit Data Set, and more. Ultimately, this project analyzed how specific factors influenced public transportation ridership, with a special focus on ride-sharing services (i.e., Uber).

### Motivation

Public transportation is one of the most important aspects of modern infrastructure. In addition to being a critical driver of economic growth and productivity, it dramatically increases quality of life within major cities by providing access to food, education and entertainment, especially for those with lower socioeconomic status. Investment in public transportation is thus a key public policy issue, especially in the context of the Infrastructure Investment and Jobs Act (P.L. 117-58), which allocates nearly $20 billion in new government spending on transportation. In fact, public transportation ridership in the U.S. had been decreasing even prior to the COVID-19 pandemic. There are many potential reasons cited for this trend, including urban sprawl, population decreases, and the introduction of ride-sharing services such as Uber. Regardless, urban planners and other policy makers need to understand how various factors affect transit ridership in order to make appropriate funding and investment decisions. Specifically, the questions of interest are: is there a trend over time after accounting for other factors, does the introduction of Uber in a city affect the ridership trend, and what potentially controllable factors influence ridership the most?

### Major Findings

Full findings and detailed methodology are available in the (PDF report)[https://github.com/razalamb1/transit-modeling/blob/main/40_output/final_report.pdf], located in the `40_output` folder of the repository. Utlimately, a linear regression was utilized in this analysis, after a hierarchical linear model failed to converge. However, the linear model showed that all cities experienced a change in their trend of ridership over time **after** Uber was introduced in that respective city. In fact, all cities except San Francisco experienced a reversal of trend (i.e., a positive trend over time before Uber is introduced and a negative trend afterwards). See this effect below.

![plot](https://github.com/razalamb1/transit-modeling/blob/main/40_output/Screen%20Shot%202022-01-04%20at%205.36.26%20PM.png)

### Directions to Recreate Data

The results from this analysis are easily verifiable. All source data (either scraped or downloaded) is available in the `00_source` folder, and the Python code used to clean and merge the data is available in the `10_code` folder. The exploratory analysis and modeling was done in R, and the requisite code can be found in the `30_analysis` folder.
