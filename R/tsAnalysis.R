#' @title tsAnalysis
#' @description saves figures
#' @return returns list of stacked
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @author David Chen
#' @importFrom  raster cellStats
#' @export

tsAnalysis <- function(region="Eastern Arctic", season="Summer"){
  ## names for later writing for rasters
  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

# time series analysis and cell statistics ------------------------------
# overall trend of seasonal sea ice habitat
# getting started
ts_sum_path <- paste0("data/BASELINE/",region, "/", season, "_", reg,"_Output/output_ct_sa_fa_sst/")
sum_files <- list.files(ts_sum_path, full.names = TRUE, pattern = "sum_")

ts_sum_stack <- lapply(sum_files, stack)

ts_sum_stack <- stack(matchExtents(ts_sum_stack))

# plotting time series data
#ts_sum_stack_df <- as.data.frame(ts_sum_stack, xy = TRUE) %>%
  #melt(id.vars = c('x','y'))

# ggplot() +
#   geom_raster(data=ts_sum_stack_df, aes(x=x, y=y, fill=value)) +
#   facet_wrap(~variable)

# # view distribution of raster values
# ggplot(ts_sum_stack_df) +
#   geom_histogram(aes(value)) +
#   facet_wrap(~variable)

# calculate cell statistics
sum_mean <- cellStats(ts_sum_stack,stat='mean', na.rm=TRUE)
sum_df <- as.data.frame(sum_mean)
names(sum_df) <- "mean_values"
sum_df$max <- cellStats(ts_sum_stack,stat='max', na.rm=TRUE)
sum_df$min <- cellStats(ts_sum_stack, stat='min', na.rm=TRUE)
sum_df$sd <- cellStats(ts_sum_stack,stat='sd', na.rm=TRUE)
sum_df$site <- region
sum_df$season <- season
sum_df$variable <- "overall_trend"
#head(sum_df)

# extract year from file names
years <- gsub(".*?([0-9]+).*", "\\1", sum_files)
sum_df$year <- paste0(years)
#head(sum_df)
#class(sum_df$year)

# plot data using ggplot
# ggplot(sum_df, aes(year, mean_values)) +
#   geom_point(colour = "SpringGreen4") +
#   ggtitle("Suitable Sea Ice Habitat (2006-2019)", subtitle = "paste0(region,", ", season, " Season")") +
#   xlab("Year") + ylab("Mean Values")

# plot all data in one ggplot
overall_trend_plot = ggplot() +
  geom_point(data =sum_df, aes(x=year, y=mean_values/100, color ="Mean")) +
  geom_line(data =sum_df, aes(x=year, y=mean_values/100, group=1, color ="Mean")) +
  geom_point(data=sum_df, aes(x=year, y=max/100, color="Max"))+
  geom_line(data=sum_df, aes(x=year, y=max/100, group=1, color="Max"))+
  geom_point(data=sum_df, aes(x=year, y=min/100, color="Min"))+
  geom_line(data=sum_df, aes(x=year, y=min/100, group=1, color="Min"))+
  geom_point(data=sum_df, aes(x=year, y=sd/100, color="SD"))+
  geom_line(data=sum_df, aes(x=year, y=sd/100, group=1, color="SD"))+
  ggtitle("", subtitle = paste0(region,", ", season, " Season")) +
  xlab('Year')+
  ylab('Trend Statistics') +
  scale_color_manual(values=c("Mean"="blue", "Max"="green", "Min"="red", "SD"="purple"))+
  labs(color= "Statistical Measures") #, caption="Silja Zimmermann, 2020")
print(overall_trend_plot)

ggsave(paste0("data/BASELINE/", region,"/", season, "_", reg,"_Output/overall_trend_plot.png"))

# trends of sea ice concentration, sea ice thickness and floe size
# getting started
ct_path <- paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/ct/")
sa_path <- paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/sa/")
fa_path <- paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/fa/")
sst_path <-paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/sst/")

ct_list <- list.files(ct_path, full.names = TRUE, pattern = "sum_")
sa_list <- list.files(sa_path, full.names = TRUE, pattern = "sum_")
fa_list <- list.files(fa_path, full.names = TRUE, pattern = "sum_")
sst_list <- list.files(sst_path, full.names = TRUE, pattern = "sum_")


ct_stack <- lapply(ct_list, stack)
ct_stack <- stack(matchExtents(ct_stack))

sa_stack <- lapply(sa_list, stack)
sa_stack <- stack(matchExtents(sa_stack))

fa_stack <- lapply(fa_list, stack)
fa_stack <- stack(matchExtents(fa_stack))

sst_stack <- lapply(ct_list, stack)
sst_stack <- stack(matchExtents(sst_stack))

# calculate cell statistics
# CT
ct_mean <- cellStats(ct_stack,stat='mean',na.rm=TRUE)
ct_df <- as.data.frame(ct_mean)
names(ct_df) <- "mean_values"
ct_max <- cellStats(ct_stack,stat='max',na.rm=TRUE)
ct_df$max <- ct_max
ct_min <- cellStats(ct_stack,stat='min', na.rm=TRUE)
ct_df$min <- ct_min
ct_sd <- cellStats(ct_stack,stat='sd',na.rm=TRUE)
ct_df$sd <- ct_sd
ct_df$site <- region
ct_df$season <- season
ct_df$variable <- "ct"
head(ct_df)

# SA
sa_mean <- cellStats(sa_stack,mean)
sa_df <- as.data.frame(sa_mean)
names(sa_df) <- "mean_values"
sa_max <- cellStats(sa_stack,stat='max',na.rm=TRUE)
sa_df$max <- sa_max
sa_min <- cellStats(sa_stack,stat='min', na.rm=TRUE)
sa_df$min <- sa_min
sa_sd <- cellStats(sa_stack,stat='sd',na.rm=TRUE)
sa_df$sd <- sa_sd
sa_df$site <- region
sa_df$season <- season
sa_df$variable <- "sa"
head(sa_df)

# FA
fa_mean <- cellStats(fa_stack,mean)
fa_df <- as.data.frame(fa_mean)
names(fa_df) <- "mean_values"
fa_max <- cellStats(fa_stack,stat='max',na.rm=TRUE)
fa_df$max <- fa_max
fa_min <- cellStats(fa_stack,stat='min', na.rm=TRUE)
fa_df$min <- fa_min
fa_sd <- cellStats(fa_stack,stat='sd',na.rm=TRUE)
fa_df$sd <- fa_sd
fa_df$site <- region
fa_df$season <- season
fa_df$variable <- "fa"
head(fa_df)

# SST
sst_mean <- cellStats(sst_stack,mean)
sst_df <- as.data.frame(sst_mean)
names(sst_df) <- "mean_values"
sst_max <- cellStats(sst_stack,stat='max',na.rm=TRUE)
sst_df$max <- sst_max
sst_min <- cellStats(sst_stack,stat='min', na.rm=TRUE)
sst_df$min <- sst_min
sst_sd <- cellStats(sst_stack,stat='sd',na.rm=TRUE)
sst_df$sd <- sst_sd
sst_df$site <- region
sst_df$season <- season
sst_df$variable <- "sst"
head(sst_df)

# extract year from row names
# CT
ct_df$year <- paste0(years)
head(ct_df)
class(ct_df$year)

# SA

sa_df$year <- paste0(years)
head(sa_df)
class(sa_df$year)

# FA

fa_df$year <- paste0(years)
head(fa_df)
class(fa_df$year)


# SST

sst_df$year <- paste0(years)
head(sst_df)
class(sst_df$year)


# plot data using ggplot
# CT
ct_plot = ggplot() +
  geom_point(data =ct_df, aes(x=year, y=mean_values, color="Mean")) +
  geom_line(data =ct_df, aes(x=year, y=mean_values, group=1, color="Mean")) +
  geom_point(data=ct_df, aes(x=year, y=max, color="Max"))+
  geom_line(data=ct_df, aes(x=year, y=max, group=1, color="Max"))+
  geom_point(data=ct_df, aes(x=year, y=min, color="Min"))+
  geom_line(data=ct_df, aes(x=year, y=min, group=1, color="Min"))+
  geom_point(data=ct_df, aes(x=year, y=sd, color="SD"))+
  geom_line(data=ct_df, aes(x=year, y=sd, group=1, color="SD"))+
  ggtitle("Sea Ice Concentration (2006-2019)", subtitle = paste0(region,", ", season, " Season")) +
  xlab('Year')+
  ylab('Cell Statistics')+ ylim(0, 1.2) +
  scale_color_manual(values=c("Mean"="green", "Max"="red", "Min"="blue", "SD"="purple"))+
  labs(color= "Statistical Measures", caption="Silja Zimmermann, 2020")
print(ct_plot)

# SA
sa_plot = ggplot() +
  geom_point(data =sa_df, aes(x=year, y=mean_values, color="Mean")) +
  geom_line(data =sa_df, aes(x=year, y=mean_values, group=1, color="Mean")) +
  geom_point(data=sa_df, aes(x=year, y=max, color="Max"))+
  geom_line(data=sa_df, aes(x=year, y=max, group=1, color="Max"))+
  geom_point(data=sa_df, aes(x=year, y=min, color="Min"))+
  geom_line(data=sa_df, aes(x=year, y=min, group=1, color="Min"))+
  geom_point(data=sa_df, aes(x=year, y=sd, color="SD"))+
  geom_line(data=sa_df, aes(x=year, y=sd, group=1, color="SD"))+
  ggtitle("Sea Ice Thickness (2006-2019)", subtitle = paste0(region,", ", season, " Season")) +
  xlab('Year')+
  ylab('Cell Statistics')+ ylim(0, 1.2) +
  scale_color_manual(values=c("Mean"="green", "Max"="red", "Min"="blue", "SD"="purple"))+
  labs(color= "Statistical Measures", caption="Silja Zimmermann, 2020")
print(sa_plot)

# FA
fa_plot = ggplot() +
  geom_point(data =fa_df, aes(x=year, y=mean_values, color="Mean")) +
  geom_line(data =fa_df, aes(x=year, y=mean_values, group=1, color="Mean")) +
  geom_point(data=fa_df, aes(x=year, y=max, color="Max"))+
  geom_line(data=fa_df, aes(x=year, y=max, group=1, color="Max"))+
  geom_point(data=fa_df, aes(x=year, y=min, color="Min"))+
  geom_line(data=fa_df, aes(x=year, y=min, group=1, color="Min"))+
  geom_point(data=fa_df, aes(x=year, y=sd, color="SD"))+
  geom_line(data=fa_df, aes(x=year, y=sd, group=1, color="SD"))+
  ggtitle("Floe Size (2006-2019)", subtitle = paste0(region,", ", season, " Season")) +
  xlab('Year')+
  ylab('Cell Statistics')+ ylim(0, 1.2) +
  scale_color_manual(values=c("Mean"="green", "Max"="red", "Min"="blue", "SD"="purple"))+
  labs(color= "Statistical Measures", caption="Silja Zimmermann, 2020")
print(fa_plot)


# SST
sst_plot = ggplot() +
  geom_point(data =sst_df, aes(x=year, y=mean_values, color="Mean")) +
  geom_line(data =sst_df, aes(x=year, y=mean_values, group=1, color="Mean")) +
  geom_point(data=sst_df, aes(x=year, y=max, color="Max"))+
  geom_line(data=sst_df, aes(x=year, y=max, group=1, color="Max"))+
  geom_point(data=sst_df, aes(x=year, y=min, color="Min"))+
  geom_line(data=sst_df, aes(x=year, y=min, group=1, color="Min"))+
  geom_point(data=sst_df, aes(x=year, y=sd, color="SD"))+
  geom_line(data=sst_df, aes(x=year, y=sd, group=1, color="SD"))+
  ggtitle("Sea Surface Temperature", subtitle = paste0(region,", ", season, " Season")) +
  xlab('Year')+
  ylab('Cell Statistics')+ ylim(0, 1.2) +
  scale_color_manual(values=c("Mean"="green", "Max"="red", "Min"="blue", "SD"="purple"))+
  labs(color= "Statistical Measures", caption="Silja Zimmermann, 2020")
print(sst_plot)


ct_sa_fa_df <- rbind(ct_df, sa_df, fa_df, sst_df)
head(ct_sa_fa_df)

ct_sa_fa_plot <- ggplot(ct_sa_fa_df, aes(x=year, y=mean_values, colour=variable)) +
  geom_point(aes(group=variable)) +
  geom_line(aes(group=variable)) +
  ggtitle("Sea Ice Concentration, Sea Ice Thickness, Floe Size, Sea Surface Temperature", subtitle =paste0(region,", ", season, " Season")) +
  xlab("Year") + ylab("Mean Values") + ylim(0, 1.2) +
  labs(color="Variable", caption="Silja Zimmermann, 2020")
print(ct_sa_fa_plot)

ct_sa_fa_plot + scale_color_manual(name="Variable",
                                   labels = c("CT",
                                              "FA",
                                              "SA",
                                              "SST"),
                                   values = c("ct" = "seagreen",
                                              "fa" = "firebrick",
                                              "sa" = "darkorange1",
                                              "sst" = "black"))

# plot data using ggplot
# overall trend, ct, sa, fa
final_plot <- ggplot() +
  geom_point(data=ct_sa_fa_df, aes(x=year, y=mean_values, group=variable, colour=variable)) +
  geom_line(data=ct_sa_fa_df, aes(x=year, y=mean_values, group=variable, colour=variable)) +
  geom_point(data=sum_df, aes(x=year, y=mean_values/100, group=variable, colour=variable)) +
  geom_line(data=sum_df, aes(x=year, y=mean_values/100, group=variable, colour=variable)) +
  ggtitle("", subtitle = paste0(region,", ", season, " Season")) +
  xlab("Year") + ylab("Mean Values") + ylim(0, 1) +
  labs(color="Variable") #, caption="Silja Zimmermann, 2020")
print(final_plot)

final_plot + scale_color_manual(name="Variable",
                                labels = c("Sea Ice Concentration",
                                           "Floe Size",
                                           "Overall Trend",
                                           "Ice Thickness"),
                                values = c("ct" = "seagreen",
                                           "fa" = "firebrick",
                                           "overall_trend" = "darkblue",
                                           "sa" = "darkorange1"))

# write data to .csv file
total_overview_df <- rbind(ct_sa_fa_df, sum_df)
head(total_overview_df)

# row.names(total_overview_df) <- NULL
# head(total_overview_df)
output_path <- paste0("data/BASELINE/",region, "/", season, "_", reg,"_Output/ts_analysis/")
write.csv(total_overview_df, file=paste0(output_path,"CellStatistics_",reg, "_", season, "_2006-2019.csv"))
return(total_overview_df)
}
