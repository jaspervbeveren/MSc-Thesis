#GENERATE STOCHASTIC WEATHER REALIZATIONS

library(tidyr)
library(ggplot2)
library(dplyr)
library(weathergenr)
input_path = "P:/11206881-climate-risk-in-cities/msc_theses/Jasper/weathergenr/extract_historical_data/"
ncfile = paste0("P:/11206881-climate-risk-in-cities/msc_theses/Jasper/weathergenr/extract_historical_data/", "era5_bangkok_1981-2011.nc")
ncdata <- readNetcdf(ncfile)

output_path <- "P:/11206881-climate-risk-in-cities/msc_theses/Jasper/weathergenr/GWS/1981-2010b/"
variables <- c("precip", "temp", "temp_min", "temp_max")
realization_num <- 5

stochastic_weather <- generateWeatherSeries(
  weather.data = ncdata$data,
  weather.grid = ncdata$grid,
  weather.date = ncdata$date,
  variable.names = variables,
  variable.labels = NULL,
  variable.units = NULL,
  sim.year.num = 30,
  sim.year.start = 1980,
  month.start = 1,
  realization.num = realization_num,
  warm.variable = "precip",
  warm.signif.level = 0.9, #to check
  warm.sample.num = 5000,
  warm.subset.criteria = NULL,
  knn.sample.num = 100,
  mc.wet.quantile = 0.3,
  mc.extreme.quantile = 0.8,
  evaluate.model = TRUE,
  evaluate.grid.num = 20,
  output.path = output_path,
  seed = NULL,
  compute.parallel = TRUE,
  num.cores = NULL
)


#CLIMATE STRESS TESTING

# Temp mean changes Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
delta_temp_mean_min <- c(4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4)
delta_temp_mean_max <- c(4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4, 4.4)

# Precip mean changes   Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
delta_precip_mean_min <- c(1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12)
delta_precip_mean_max <- c(1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12, 1.12)

# Precip variance changes   Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
delta_precip_variance_min <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
delta_precip_variance_max <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0) 

# Number of incremental step changes for precip and temp variables
precip_step_num <- 2
temp_step_num <- 2

precip_mean_steps <- sapply(1:12, function(m)
  seq(delta_precip_mean_min[m], delta_precip_mean_max[m],
      length.out = precip_step_num))

precip_variance_steps <- sapply(1:12, function(m)
  seq(delta_precip_variance_min[m], delta_precip_variance_max[m],
      length.out = precip_step_num))

temp_mean_steps <- sapply(1:12, function(m)
  seq(delta_temp_mean_min[m], delta_temp_mean_max[m],
      length.out = temp_step_num))

df1 <- as.data.frame(precip_mean_steps) %>% mutate(level = 1:n(), 
                                                   variable = "precip_mean", .before = 1)
df2 <- as.data.frame(precip_variance_steps) %>% mutate(level = 1:n(), 
                                                       variable = "precip_variance", .before = 1)
df3 <- as.data.frame(temp_mean_steps) %>% mutate(level = 1:n(), 
                                                 variable = "temp_mean", .before = 1)
df <- bind_rows(df1, df2, df3) %>% gather(month, value, V1:V12) %>%
  mutate(month = factor(month, levels = paste0("V",1:12), labels = 1:12))

p <- ggplot2::ggplot(df, aes(x = month, y = value, group = level, color = level)) +
  facet_wrap(. ~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(x="month", y = "delta factor") +
  scale_color_distiller(palette = "Set1") +
  guides(color = "none")

p

# Stress test matrix
strtest_matrix <- tidyr::expand_grid(stoc_ind = 1:realization_num,
                                     precip_ind = 1:precip_step_num, temp_ind = 1:temp_step_num)

# Total number of scenarios
smax <- nrow(strtest_matrix)

# Stress test delta factors for each variable/climate statistic
strtest_matrix_precip_mean <- precip_mean_steps[strtest_matrix$precip_ind, ]
strtest_matrix_precip_variance <- precip_variance_steps[strtest_matrix$precip_ind, ]
strtest_matrix_temp_mean <- temp_mean_steps[strtest_matrix$temp_ind, ]

# Write stress test matrices to file (optional)

write.csv(strtest_matrix, 
          paste0(output_path, "strtest_matrix.csv"), row.names = FALSE)
write.csv(strtest_matrix_precip_mean, 
          paste0(output_path, "strtest_matrix_precip_mean.csv"), row.names = FALSE)
write.csv(strtest_matrix_precip_variance, 
          paste0(output_path, "strtest_matrix_precip_variance.csv"), row.names = FALSE)
write.csv(strtest_matrix_temp_mean, 
          paste0(output_path, "strtest_matrix_temp_mean.csv"), row.names = FALSE)

#Finally, lets generate the stress test input data

# Read-in resampled dates & date series (from csv files included with the package)
resampled_dates <- read.csv(file = "P:/11206881-climate-risk-in-cities/msc_theses/Jasper/weathergenr/GWS/1981-2010b/historical/resampled_dates.csv",
                            colClasses = "Date")
sim_dates <- read.csv(file = "P:/11206881-climate-risk-in-cities/msc_theses/Jasper/weathergenr/GWS/1981-2010b/historical/sim_dates.csv", 
                      colClasses = "Date")[[1]]

# resampled_dates <- read.csv(file = "C:/Users/beveren/msc_thesis/weathergenr/GWS/1981-2011/historical/resampled_dates.csv", colClasses = "Date")
# sim_dates <- read.csv(file = "C:/Users/beveren/msc_thesis/weathergenr/GWS/1981-2011/historical/sim_dates.csv", colClasses = "Date")

# Use results from generateWeatherSeries function output
# resampled_dates <- stochastic_weather$resampled
# sim_dates <- stochastic_weather$dates

# progress bar (optional)
pb = txtProgressBar(min = 1, max = smax, initial = 0, style = 3)
for (s in 1:smax) {
  
  setTxtProgressBar(pb,s)
  
  # Find the current scenario indices for the stochastic realization and delta factors
  stoc_ind <- strtest_matrix$stoc_ind[s]
  
  # Obtain stochastic series by re-ordering historical data
  day_order <- match(resampled_dates[[stoc_ind]], ncdata$date)
  rlz_historical <- lapply(ncdata$data, function(x) x[day_order,])
  
  # Apply climate changes to climate data
  rlz_future <- imposeClimateChanges(
    climate.data = rlz_historical,
    climate.grid = ncdata$grid,
    sim.dates = sim_dates,
    change.factor.precip.mean = strtest_matrix_precip_mean[s,],
    change.factor.precip.variance = strtest_matrix_precip_variance[s,],
    change.factor.temp.mean = strtest_matrix_temp_mean[s,],
    change.type.temp = "step", #change to steady?
    change.type.precip = "FALSE") #change to steady?
  
  # Save to netcdf file
  writeNetcdf(
    data = rlz_historical,
    coord.grid = ncdata$grid,
    output.path = output_path,
    origin.date =  stochastic_weather$dates[1],
    calendar.type = "noleap",
    nc.template.file = ncfile,
    nc.compression = 4,
    nc.spatial.ref = "spatial_ref",
    nc.file.prefix = "variability",
    nc.file.suffix = s)
  
  # Save to netcdf file
  writeNetcdf(
    data = rlz_future,
    coord.grid = ncdata$grid,
    output.path = output_path,
    origin.date =  stochastic_weather$dates[1],
    calendar.type = "noleap",
    nc.template.file = ncfile,
    nc.compression = 4,
    nc.spatial.ref = "spatial_ref",
    nc.file.prefix = "climx",
    nc.file.suffix = s)
}
close(pb)
