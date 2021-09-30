
####--------------------------------------------------------####
####----  Exercises: Comp. Statistician / R Developper  ----####
####--------------------------------------------------------####


###--- Task 1: Transform the dataset below in order to use the plotting code below.
###----------: Explanation: none, IRS, ITN, IRS+ITN are 4 different intervention types.
###----------: The values in the dataset are 'PR' (prevalence)

rm(list = ls())
dat = read.delim(text = "zone	population	year	none	IRS	ITN	IRS+ITN
Zone 1	692089	2005	0.41	0.40	0.39	0.40
Zone 1	692089	2006	0.37	0.35	0.35	0.35
Zone 1	692089	2007	0.31	0.29	0.29	0.29
Zone 1	692089	2008	0.28	0.26	0.26	0.26
Zone 1	692089	2009	0.25	0.22	0.23	0.22
Zone 1	692089	2010	0.19	0.17	0.18	0.18
Zone 1	692089	2011	0.15	0.13	0.13	0.14
Zone 1	692089	2012	0.11	0.10	0.10	0.10
Zone 1	692089	2013	0.10	0.09	0.08	0.09
Zone 1	692089	2014	0.12	0.11	0.11	0.12
Zone 1	692089	2015	0.16	0.16	0.16	0.17
Zone 1	692089	2016	0.20	0.21	0.22	0.21
Zone 1	692089	2017	0.24	0.24	0.26	0.25
Zone 1	692089	2018	0.18	0.18	0.18	0.18
Zone 1	692089	2019	0.09	0.09	0.08	0.09
Zone 1	692089	2020	0.09	0.09	0.07	0.07
Zone 1	692089	2021	0.13	0.08	0.08	0.06
Zone 1	692089	2022	0.18	0.07	0.10	0.05
Zone 2	324469	2005	0.26	0.26	0.26	0.26
Zone 2	324469	2006	0.21	0.20	0.20	0.20
Zone 2	324469	2007	0.16	0.15	0.15	0.15
Zone 2	324469	2008	0.12	0.13	0.12	0.12
Zone 2	324469	2009	0.09	0.10	0.09	0.09
Zone 2	324469	2010	0.07	0.07	0.07	0.07
Zone 2	324469	2011	0.04	0.05	0.04	0.04
Zone 2	324469	2012	0.03	0.03	0.03	0.03
Zone 2	324469	2013	0.02	0.03	0.03	0.02
Zone 2	324469	2014	0.03	0.03	0.03	0.03
Zone 2	324469	2015	0.05	0.05	0.05	0.05
Zone 2	324469	2016	0.09	0.09	0.09	0.09
Zone 2	324469	2017	0.11	0.11	0.11	0.11
Zone 2	324469	2018	0.10	0.10	0.10	0.10
Zone 2	324469	2019	0.05	0.05	0.05	0.05
Zone 2	324469	2020	0.05	0.05	0.04	0.04
Zone 2	324469	2021	0.07	0.04	0.04	0.03
Zone 2	324469	2022	0.10	0.03	0.05	0.02", sep = "\t", header = T)


plot1 = ggplot( dat2 ) +
  geom_line(aes(y = PR, x = year, group = intervention , col = intervention), lwd = 1.5) +
  facet_wrap(~ zone ) +
  theme_minimal() +
  scale_color_viridis_d( end = .9, begin = .2)

print( plot1 )

### Your Response (#1)

library(tidyr)
dat2 <- gather(dat, intervention, PR, none:IRS.ITN)



###--- Task 2: The dataset 'dat' contains prevalence values for a country with two zones.
###---       : Your task is to write a custom function that outputs the national prevalence
###---       : (population-weighted mean of the two zones), by year and by intervention.
###---       : Then use the function to compute the values for each intervention from 2020:2022.
###----------: examples:
# popmean( dat, intervention = "IRS", years = 2020:2022)
# popmean( dat, intervention = "ITN", years = 2020:2022)


### Your Response (#2)

#' calculate national prevalence
#'
#' @param data dataframe
#' @param intervention intervention
#' @param years range of years
#'
#' @return dataframe with two columns "years" and "PR"
popmean <- function(data, intervention, years) {

  # filter for relevant data
  data <- subset(data, year %in% years)
  data <- data[,c("zone", "population", "year", intervention)]
  colnames(data) <- c("zone", "population", "year", "PR")

  # determine total polulation
  total <- sum(data$population[which(!duplicated(data$zone))])

  # replace PR by weighted PR and sum
  data$PR <- data$population * data$PR / total
  data <- aggregate(data$PR, list(data$year), sum)
  colnames(data) <- c("year", "PR")

  return( data )
}



###--- Task 3: Debug and format according to coding best practices the 'p_reduct' function below,
###---       : and use it on the 'dat3' object.
###--- run the following lines to include this function in your 'global environment'
p_reduct = function( dat3 , baseyear, print = F ){
  #' calculates the percent reduction based on some baseyear
  #' @param baseyear a year against which to make comparisons
  #' @param dat a dataset with columns "value" and "year"

  dat3 = dat3 %>%  mutate(
    redu = ( value[ year == basyear ] - value ) / value[ year == baseyear ]
  )

  if(print) print(dat2)
  return( dat3 )
}


## use the 'p_reduct' function on this object
dat3 = cbind.data.frame( PR = sort( signif( runif(11,0,100), 3),decreasing = T)
                         , year = 2020:2030 )

## notice that the function fails to run
p_reduct( dat3, baseyear = 2020, print = T)


## Question: how do you change the code so it works?

### Your Response (#3)

library(tidyverse)

#' calculates the percent reduction based on some baseyear
#'
#' @param dat a dataset with columns "value" and "year"
#' @param baseyear a year against which to make comparisons
#' @param print Should the output dataset be printed?
#'
#' @return a dataset with columns "value", "year" and "redu"
#'
#' @import tidyverse
p_reduct = function( dat , baseyear, print = F ){

  colnames(dat) <- c("value", "year")
  attach(dat)

  dat = dat %>% mutate(
    redu = 100 * ( value[ year == baseyear ] - value ) / value[ year == baseyear ]
  )

  if(print) {
    print(dat)
  }

  return( dat )
}



### Task 4: Put the function from task 2 in an R file.
###-------: Add unit test(s) for your function.
###-------:    (i.e. create a separate file with unit tests - any test library is fine.
###-------:    If you're not sure, use testthat.)
###-------: Create a small Gitlab project containing an R script with the
###-------:    link to your project in the answer line.

### Your Response (#4)

# https://github.com/lars20070/task4



### Task 5: Merge the two (messy) datasets to obtain a dataset with 18 observations and 4 columns.
###-------: What functions can you use to match the names?
file1 = read.delim(text="REGION	DISTRICT	SMC
GREATER ACCRA	ADENTA MUNICIPAL	1
GREATER ACCRA	LEDZOKUKU MUNICIPAL	1
NORTHERN EAST	CHEREPONI	1
UPPER WEST	WA EAST	0
UPPER WEST	WA MUNICIPAL	0
NORTHERN	SABOBA	0
UPPER WEST	JIRAPA	1
NORTHERN	GUSHEGU	1
UPPER WEST	SISSALA WEST	0
NORTHERN	NANUMBA NORTH	0
UPPER WEST	LAMBUSSIE-KARNI	1
UPPER EAST	BOLGATANGA MUNICIPAL	1
NORTHERN	NANUMBA SOUTH	1
BONO	WENCHI MUNICIPAL	0
UPPER EAST	BONGO	0
AHAFO	TANO SOUTH MUNICIPAL	0
UPPER EAST	KASENA NANKANA WEST	1
UPPER WEST	WA WEST	1", header = T, sep = "\t")

file2 = read.delim( text ="adm1	adm2	irs
Greater-Accra	Adenta Municipal	1
Greater-Accra	Ledzokuku Municipal	1
Upper-West	Lambussie-Karni	1
Ahafo	Tano South Municipal	0
Upper-East	Kasena Nankana West	0
Northern	Saboba	0
Upper-West	Jirapa	1
Northern	Gushegu	1
Upper-West	Sissala West	0
Northern-East	Chereponi	1
Upper-West	Wa East	1
Upper-West	Wa Municipal	0
Northern	Nanumba South	1
Bono	Wenchi Municipal	1
Upper-East	Bongo	0", header = T, sep = "\t")

#### Your response (#5)

# make column names consistent
colnames(file2) <- c('REGION', 'DISTRICT', 'IRS')

# capitalize strings and remove hyphens
cleanup <- function(name) {
  name <- toupper(name)
  name <- sub('-', ' ', name)

  return(name)
}

# clean up geographic names
file1$REGION <- cleanup(file1$REGION)
file1$DISTRICT <- cleanup(file1$DISTRICT)
file2$REGION <- cleanup(file2$REGION)
file2$DISTRICT <- cleanup(file2$DISTRICT)

# merge dataframes
file3 <- merge(file1, file2, by=c('REGION', 'DISTRICT'), all=T)



### Task 6: Evaluate the following function and propose an improved version of it.

### the define_changeHS function uses the 'deploy' function below
deploy <- function(y1 = 2000, y2 = NULL,
                   m1 = 5, m2 = NULL,
                   d1 = 5, d2 = NULL,
                   every = 1, interval = "year",
                   SIMSTART = "1918-01-01") {
  #' Deployment function
  #' @param y1 year of the first date (surveys starting from year y1)
  #' @param m1 month of the first date
  #' @param d1 day of the first date
  #' @param y2 year of the end date (surveys continuing until year y2)
  #' @param m2 month of the end date
  #' @param d2 day of the end date
  #' @param every interval size
  #' @param interval interval size (days, weeks, )
  #' @param SIMSTART  Starting date of the simulations in the format "yyyy-mm-dd"
  #' @example deploy( y1 = 2020, y2 = 2022, m1 = 5, m2 = 4) will deploy in
  #' 5/2020, 6/2020, 7/2020, 8/2020, 9/2020, ... , 2/2023, 3/2023, 4/2022
  #' @example deploy( y1 = 2020, y2 = 2022, m1 = 5, m2 = 7) will deploy only in
  #' 5/2020, 6/2020, 7/2020, 5/2021, 6/2021, 7/2021, 5/2022, 6/2022, 7/2022

  ##-- assumptions when missing
  if( is.null(y2) ) y2 = y1
  if( is.null(m2) ) m2 = m1
  if( is.null(d2) ) d2 = d1

  ##-- warning if NA values
  if( is.na( m1 ) ) stop("month value is NA")
  if( is.na( d1 ) ) stop("day value is NA")
  if( is.na( y1 ) ) stop("year value is NA")

  ##-- warning if non-sensical values
  if( y2 < y1 ) stop("error: y2 < y1")
  if( m2 < m1 ){
    message(
      paste(  "Assuming deployments every", every,interval, "from"
              , m1, "-", y1, "to", m2, "-", y2) )
    #every = 1; interval = "month"
  }

  o2 <- NULL
  if (interval == "quarter") {
    m1 <- 1
    m2 <- 12
  }

  #### if the start month is before the end month
  if (m1 < m2) {
    for (year in y1:y2) {
      o2 <- c(
        o2,
        seq(as.Date(paste(year, m1, d1, sep = "-")),
            as.Date(paste(year, m2, d2, sep = "-")),
            by = paste(every, interval)
        )
      )
    }
  } # end year loop

  # m1 = 10 ; m2 = 10; y1 = 2020; y2 = 2022; d1 = 5; d2 = 5;
  ### otherwise
  if (m1 >= m2) {
    o2 <-
      seq(as.Date(paste(y1, m1, d1, sep = "-")),
          as.Date(paste(y2, m2, d1, sep = "-")),
          by = paste(every, interval)
      )
  }

  ### returning unique dates
  y   <- as.Date(sort(unique(o2)), origin = "1970-01-01")
  out <- as.numeric(round((y - as.Date(SIMSTART)) / 5 + 1, 0))

  return(list(dates = y, timestep = out))
} # end deploy function

#### Your response (#6)

#' Deployment function
#'
#' @param y1 year of the first date (surveys starting from year y1)
#' @param m1 month of the first date
#' @param d1 day of the first date
#' @param y2 year of the end date (surveys continuing until year y2)
#' @param m2 month of the end date
#' @param d2 day of the end date
#' @param every interval size
#' @param interval interval unit ('day', 'week', 'month', 'quarter', 'year')
#' @param SIMSTART starting date of the simulations in the format "yyyy-mm-dd"
#'
#' @return a date sequence and a 'timestep' sequence
#'
#' @example deploy( y1 = 2020, y2 = 2022, m1 = 5, m2 = 4, d1 = 1, d2 = 4)
#' @example deploy( y1 = 2020, y2 = 2022, m1 = 5, m2 = 7, d1 = 3, d2 = 7)
#'
deploy <- function(y1 = 2000, y2 = NULL,
                   m1 = 5, m2 = NULL,
                   d1 = 5, d2 = NULL,
                   every = 1, interval = "month",
                   SIMSTART = "1918-01-01") {

  # default dates (in case NULL is explicitly passed to the function)
  start.default <- as.Date('2000-05-05')
  start.sim.default <- as.Date('1918-01-01')

  # construct step size
  step.size <- paste(every, interval)

  # check for NAs
  if (is.na(y1)) stop("First year value is NA.")
  if (is.na(m1)) stop("First month value is NA.")
  if (is.na(d1)) stop("First day value is NA.")
  if (is.na(y2)) stop("Second year value is NA.")
  if (is.na(m2)) stop("Second month value is NA.")
  if (is.na(d2)) stop("Second day value is NA.")

  # construct dates
  start <- as.Date(paste(y1, m1, d1, sep="-"))
  end <- as.Date(paste(y2, m2, d2, sep="-"))
  start.sim <- as.Date(SIMSTART)

  # check order of dates
  if (start > end) stop("The first date is after the second.")

  # check for NULL and order of dates
  if (is.null(start.sim)) start <- start.sim.default
  if (is.null(start)) start <- start.default
  if (is.null(end)) end <- start

  # construct time sequence
  ts <- seq(start, end, by=step.size)

  # construct simulation time sequence
  ts.sim <- as.numeric(round((ts - start.sim)/5 + 1, 0))    # Not sure why a 5-day step size is hard-coded.

  return(list(dates = ts, timestep = ts.sim))
}

