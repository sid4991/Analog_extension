library(tidyverse, warn.conflicts = FALSE)
library(future, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(furrr, warn.conflicts = FALSE)
library(agclimtools, warn.conflicts = FALSE)
library(reshape2)
options(dplyr.summarise.inform = FALSE)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 9)
## Final dataframe structure would be summarized by year
## Year | Lat | Lon | Model | Climate_proj | hsd | gdd | temp_00 | temp 01|  ... | temp_24 | dry spell | Wet Spell

longest_spell <- function(precip, cutoff){
  run <- rle(precip>cutoff)
  wet <- max(run$lengths[run$values == TRUE])
  dry <- max(run$lengths[run$values == FALSE])
  return(data.frame(dry = dry, wet = wet))
}

growing_spell <- function(tmin, cutoff){
  run <- rle(tmin>cutoff)
  growing_season <- max(run$lengths[run$values == TRUE])
  return(data.frame(growing_season = growing_season))
}

analog <- function(file_path){

  print(file_path)

  path_split <- str_split(file_path, pattern = "/", simplify = TRUE)

  name_split <- str_split(path_split[,3], pattern = "_", simplify = TRUE)

  lat = as.numeric(name_split[,2])
  lon = as.numeric(name_split[,3])

  df <-
    read_binary(paste0("/data/adam/data/metdata/VIC_ext_maca_v2_binary_westUSA/", file_path), num_vars = 8) %>%
    mutate(year = year(date),
           doy = yday(date),
           month = month(date)) %>%
    add_sunrise_sunset(lat = lat)

df %>%
    expand_grid(hour = seq(ceiling(min(df$sunrise)), floor(max(df$sunset)))) %>%
    mutate(temp = temp_after_sunrise(hour, tmin, tmax, sunrise, sunset)) %>%
    group_by(year,month,date,tmax, tmin, precip) %>%
    summarise(hours_above_th = sum(temp > 32),
              hours_above_th1 = sum(temp >0 & temp < 7)) %>%
    mutate(gdd_gen = (tmax + tmin)/2) %>%
    group_by(year, month) %>%
    summarise(hsd_32 = sum(tmax > 32),
              hsd_35 = sum(tmax > 35),
              gdd_gen = sum(if_else(gdd_gen < 0, 0, gdd_gen)),
              heat_dh_32 = sum(hours_above_th),
              heat_dh_7 = sum(hours_above_th1),
              Tmax = median(tmax),
              Tmin = median (tmin),
              Precip = sum(precip),
              growing_spell(tmin,0),
              longest_spell(precip, 1))  %>%
    mutate(lat = lat,
           lon = lon,
           model = path_split[, 1],
           climate_proj = path_split[, 2])
}

plan(multicore)

models <- list.dirs("/data/adam/data/metdata/VIC_ext_maca_v2_binary_westUSA/", full.names = FALSE, recursive = FALSE)
#models <- models[16:17]
file_name <- read_lines("/data/project/agaid/AnalogData_Sid/cdl_vic_points.txt")
#file_name <- read_lines("/data/project/agaid/AnalogData_Sid/cdl_vic_points.txt",skip=30000,n_max = 22700)
climate_proj <- c("rcp45","rcp85")
climate_proj <- climate_proj[1]
file_path <- expand_grid(models, climate_proj, file_name) %>%
  mutate(file_path = paste(models, climate_proj, file_name, sep = "/")) %>%
  pull(file_path)
existing_file_paths <- file_path[file.exists(file.path("/data/adam/data/metdata/VIC_ext_maca_v2_binary_westUSA/", file_path))]

args = commandArgs(trailingOnly=TRUE)
a_future_loc <- existing_file_paths[as.numeric(args[1])]
print(args)
print(a_future_loc)
path_split1 <- str_split(a_future_loc, pattern = "/", simplify = TRUE)
name_split1 <- str_split(path_split1[,3], pattern = "_", simplify = TRUE)

df <- analog(a_future_loc)
df$location <- paste0(df$lat, "_", df$lon)
Points_2996_map <- read.csv("/data/project/agaid/AnalogData_Sid/F_V_CDL_category_point_50k.csv")
df <- merge(df,Points_2996_map,by = "location")
df <- recast(df, location + year + lat + lon + model + climate_proj + State_County  ~ variable + month, id.var = c("location","year", "lat", "lon", "model", "climate_proj","State_County", "month")) 
df <- df %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

df[, "Pr_max"] <- apply(df[, 92:103], 1, max)
df[, "Pr_min"] <- apply(df[, 92:103], 1, min)
df$Koppen <- df$Pr_max - df$Pr_min
df[, "Pr_sd"] <- apply(df[, 92:103], 1, sd)
df[, "Pr_sum"] <- apply(df[, 92:103], 1, sum)

df$DryDays_Spring = rowSums(df[,c(118:120)])
df$DryDays_Winter = rowSums(df[,c(116,117,127)])
df$WetDays_Spring = rowSums(df[,c(130:132)])
df$WetDays_Winter = rowSums(df[,c(128,129,139)])


df[, "Heat_Stress_Days_Summer_32"] <- apply(df[, 13:15], 1, sum)
df[, "Heat_Degree_Hours_Summer_32"] <- apply(df[, 49:51], 1, sum)
df$GDD_Calendar = rowSums(df[,c(32:43)])
df$Season_Length = rowSums(df[,c(104:115)])
df$Chill_Hours_O_M = rowSums(df[,c(56:58,65:67)])

df[, "Tmax"] <- apply(df[, 68:79], 1, median)
df[, "Tmin"] <- apply(df[, 80:91], 1, median)

df <- df[-c(8:139)]
out_dir<- "/home/siddharth.chaudhary/CONUS_Model_52k_rcp_45_01_13_1/"
write.csv(df, paste0(out_dir,paste0(path_split1[,3],"_",path_split1[,2],"_",path_split1[,1]),".csv"))
