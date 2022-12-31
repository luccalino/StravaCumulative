# Loading packages (install if not yet installed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, egg, httr, jsonlite, yaml, 
               lubridate, directlabels, ggpubr, grid, RColorBrewer, svDialogs,
               gghighlight, devtools, plyr, ggpubr)

# Get ID and Token
client_id <- dlgInput("Enter your Strava ID", "i.e. 12345")$res
client_secret <- dlgInput("Enter your Strava Secret", "")$res

credentials <- data.frame(client_id = as.numeric(client_id),
                          client_secret = as.character(client_secret))

token <- oauth2.0_token(
  endpoint = oauth_endpoint(
    request = NULL,
    authorize = 'https://www.strava.com/oauth/authorize',
    access = 'https://www.strava.com/oauth/token'
  ),
  app = oauth_app('strava', credentials$client_id, credentials$client_secret),
  scope = 'activity:read_all',
  as_header = F
)

data_list <- list()

i <- 1

done <- FALSE

while (!done) {
  req <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i)
  )
  data_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
  if (length(content(req)) < 200) {
    done <- TRUE
  } else {
    i <- i + 1
  }
}

data <- rbind_pages(data_list)

# Select data
rides <- data[grepl("Ride", data[["type"]]), ]
runs <- data[grepl("Run", data[["type"]]), ]

types <- c("runs", "rides")

plot_list = list()

stats <- c("elevation","distance")

stats_list <- list()

for (t in 1:length(types)) {

    plot_data <- get(types[t])
    
    plot_data <- plot_data %>%
      dplyr::mutate(start_date = as_date(start_date),
                    year = year(start_date),
                    day_of_year = yday(start_date),
                    month = month(start_date),
                    week = week(start_date))
    
    plot_data <- plot_data %>%
      dplyr::group_by(year) %>%
      dplyr::arrange(start_date) %>%
      dplyr::mutate(cumulative_elevation = cumsum(total_elevation_gain)) %>%
      dplyr::mutate(cumulative_distance = cumsum(distance)) %>%
      dplyr::ungroup() 
    
    ytick_ele_100 <- round(max(plot_data$cumulative_elevation),0)
    ytick_ele_25 <- round(ytick_ele_100/4,0)

    ytick_dist_100 <- round(max(plot_data$cumulative_distance/1000),0)
    ytick_dist_25 <- round(ytick_dist_100/4,0)
    
    scaleFUN <- function(x) sprintf("%.0f", x)
    
    col_list <- brewer.pal(n = length(unique(plot_data$year)), name = "Oranges")
    
    ydown = 15
    ydown_month = 32
    ydown_line = 33
    
      ele <- plot_data %>%
        ggplot(aes(x = day_of_year, y = cumulative_elevation, color = factor(year))) +
        geom_vline(data = plot_data, aes(xintercept = yday("2018-03-01")), colour = "black", size = 0.4) +
        geom_vline(data = plot_data, aes(xintercept = yday("2018-06-01")), colour = "black", size = 0.4) + 
        geom_vline(data = plot_data, aes(xintercept = yday("2018-09-01")), colour = "black", size = 0.4) +
        geom_vline(data = plot_data, aes(xintercept = yday("2018-12-01")), colour = "black", size = 0.4) +
        geom_segment(data = plot_data, aes(x = yday("2018-01-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-01-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-02-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-02-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-04-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-04-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-05-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-05-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-07-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-07-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-08-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-08-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-10-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-10-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-11-01"), y = -ytick_ele_100/ydown_line, xend = yday("2018-11-01"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-12-31"), y = -ytick_ele_100/ydown_line, xend = yday("2018-12-31"), yend = ytick_ele_100+ytick_ele_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_hline(data = plot_data, aes(yintercept = 0), colour = "black", size = 0.4) +
        annotation_custom(textGrob("J", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-01-16"), 
                          xmax = yday("2018-01-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("F", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-02-15"), 
                          xmax = yday("2018-02-15"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("M", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-03-16"), 
                          xmax = yday("2018-03-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("A", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-04-16"), 
                          xmax = yday("2018-04-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("M", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-05-16"), 
                          xmax = yday("2018-05-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("J", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-06-16"), 
                          xmax = yday("2018-06-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("J", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-07-16"), 
                          xmax = yday("2018-07-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("A", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-08-16"), 
                          xmax = yday("2018-08-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("S", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-09-16"), 
                          xmax = yday("2018-09-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("O", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-10-16"), 
                          xmax = yday("2018-10-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("N", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-11-16"), 
                          xmax = yday("2018-11-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("D", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-12-16"), 
                          xmax = yday("2018-12-16"), 
                          ymin = -ytick_ele_100/ydown_month, 
                          ymax = -ytick_ele_100/ydown_month) +
        annotation_custom(textGrob("Winter", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-02-01")/2), 
                          xmax = (yday("2018-02-01")/2), 
                          ymin = -ytick_ele_100/ydown, 
                          ymax = -ytick_ele_100/ydown) +
        annotation_custom(textGrob("Spring", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-06-01")-yday("2018-03-01"))/2+yday("2018-03-01"), 
                          xmax = (yday("2018-06-01")-yday("2018-03-01"))/2+yday("2018-03-01"), 
                          ymin = -ytick_ele_100/ydown, 
                          ymax = -ytick_ele_100/ydown) +
        annotation_custom(textGrob("Summer", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-09-01")-yday("2018-06-01"))/2+yday("2018-06-01"), 
                          xmax = (yday("2018-09-01")-yday("2018-06-01"))/2+yday("2018-06-01"), 
                          ymin = -ytick_ele_100/ydown, 
                          ymax = -ytick_ele_100/ydown) +
        annotation_custom(textGrob("Fall", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-12-01")-yday("2018-09-01"))/2+yday("2018-09-01"), 
                          xmax = (yday("2018-12-01")-yday("2018-09-01"))/2+yday("2018-09-01"), 
                          ymin = -ytick_ele_100/ydown, 
                          ymax = -ytick_ele_100/ydown) +
        scale_x_continuous(breaks = NULL, lim = c(0, 400)) +
        geom_line() +
        scale_y_continuous(labels = scaleFUN, limits = c(0-ytick_ele_25/8, ytick_ele_100+ytick_ele_25/8), breaks = seq(0, ytick_ele_100, by = ytick_ele_100/4)) +
        scale_color_manual(values = c(col_list)) +
        gghighlight::gghighlight(year >= min(plot_data$year)) +
        labs(title = "") +
        theme_minimal() +
        theme(legend.position = "none") +
        xlab("") +
        ylab("") + 
        geom_dl(aes(label = paste0(" ",year)), method = list(dl.combine("last.points")), cex = 0.2) 
      
      dist <- ggplot(data = plot_data, aes(x = day_of_year, y = cumulative_distance/1000, color = factor(year))) +
        geom_hline(data = plot_data, aes(yintercept = 0), colour = "black", size = 0.4) +
        geom_vline(data = plot_data, aes(xintercept = yday("2018-03-01")), colour = "black", size = 0.4) +  
        geom_vline(data = plot_data, aes(xintercept = yday("2018-06-01")), colour = "black", size = 0.4) + 
        geom_vline(data = plot_data, aes(xintercept = yday("2018-09-01")), colour = "black", size = 0.4) +
        geom_vline(data = plot_data, aes(xintercept = yday("2018-12-01")), colour = "black", size = 0.4) +
        geom_segment(data = plot_data, aes(x = yday("2018-01-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-01-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-02-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-02-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-04-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-04-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-05-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-05-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-07-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-07-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-08-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-08-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-10-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-10-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-11-01"), y = -ytick_dist_100/ydown_line, xend = yday("2018-11-01"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        geom_segment(data = plot_data, aes(x = yday("2018-12-31"), y = -ytick_dist_100/ydown_line, xend = yday("2018-12-31"), yend = ytick_dist_100+ytick_dist_100/ydown_line), 
                     colour = "grey", size = 0.25) +
        annotation_custom(textGrob("J", gp = gpar(col = "black", fontsize = 11)),
                          xmin = yday("2018-01-16"),
                          xmax = yday("2018-01-16"),
                          ymin = -ytick_dist_100/ydown_month,
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("F", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-02-15"), 
                          xmax = yday("2018-02-15"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("M", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-03-16"), 
                          xmax = yday("2018-03-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("A", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-04-16"), 
                          xmax = yday("2018-04-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("M", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-05-16"), 
                          xmax = yday("2018-05-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("J", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-06-16"), 
                          xmax = yday("2018-06-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("J", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-07-16"), 
                          xmax = yday("2018-07-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("A", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-08-16"), 
                          xmax = yday("2018-08-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("S", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-09-16"), 
                          xmax = yday("2018-09-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("O", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-10-16"), 
                          xmax = yday("2018-10-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("N", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-11-16"), 
                          xmax = yday("2018-11-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("D", gp = gpar(col = "black", fontsize = 11)), 
                          xmin = yday("2018-12-16"), 
                          xmax = yday("2018-12-16"), 
                          ymin = -ytick_dist_100/ydown_month, 
                          ymax = -ytick_dist_100/ydown_month) +
        annotation_custom(textGrob("Winter", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-02-01")/2), 
                          xmax = (yday("2018-02-01")/2), 
                          ymin = -ytick_dist_100/ydown, 
                          ymax = -ytick_dist_100/ydown) +
        annotation_custom(textGrob("Spring", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-06-01")-yday("2018-03-01"))/2+yday("2018-03-01"), 
                          xmax = (yday("2018-06-01")-yday("2018-03-01"))/2+yday("2018-03-01"), 
                          ymin = -ytick_dist_100/ydown, 
                          ymax = -ytick_dist_100/ydown) +
        annotation_custom(textGrob("Summer", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-09-01")-yday("2018-06-01"))/2+yday("2018-06-01"), 
                          xmax = (yday("2018-09-01")-yday("2018-06-01"))/2+yday("2018-06-01"), 
                          ymin = -ytick_dist_100/ydown, 
                          ymax = -ytick_dist_100/ydown) +
        annotation_custom(textGrob("Fall", gp = gpar(col = "black", fontsize = 11, fontface = "bold")), 
                          xmin = (yday("2018-12-01")-yday("2018-09-01"))/2+yday("2018-09-01"), 
                          xmax = (yday("2018-12-01")-yday("2018-09-01"))/2+yday("2018-09-01"), 
                          ymin = -ytick_dist_100/ydown, 
                          ymax = -ytick_dist_100/ydown) +
        scale_x_continuous(breaks = NULL, lim = c(0, 400)) +
        geom_line() +
        scale_y_continuous(labels = scaleFUN, limits = c(0-ytick_dist_25/8, ytick_dist_100+ytick_dist_25/8), breaks = seq(0, ytick_dist_100, by = ytick_dist_100/4)) +
        scale_color_manual(values = c(col_list)) +
        gghighlight::gghighlight(year >= min(plot_data$year)) +
        labs(title = "") +
        theme_minimal() +
        theme(legend.position = "none") +
        xlab("") +
        ylab("") + 
        geom_dl(aes(label = paste0(" ",year)), method = list(dl.combine("last.points")), cex = 0.2) 
      
      stats_list = list(ele, dist)

  plot_list[[t]] = stats_list
  
}

# Plotting
arranged_figure <- ggpubr::ggarrange(plot_list[[1]][[1]], plot_list[[1]][[2]], plot_list[[2]][[1]], plot_list[[2]][[2]],
                                       labels = c("Running: Elevation (m)", "Running: Distance (km)", "Riding: Elevation (m)", "Riding: Distance (km)"),
                                       nrow = 2, ncol = 2, common.legend = TRUE, legend = "none")

g <- annotate_figure(arranged_figure,
                bottom = text_grob(paste0("Note: Last ",as.numeric(format(Sys.Date(), "%Y"))-min(plot_data$year)+1," years highlighted as of ",Sys.Date(),"."), color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

# Saving figure
ggsave(plot = g, 
       width = 350, 
       height = 350, 
       units = "mm", 
       dpi = 300, 
       bg = "white",
       filename = "output/CumulativeStatistics.png")



