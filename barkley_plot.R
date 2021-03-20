## libraries
library(Cairo)      ## prettier output
library(data.table) ## data wrangling
library(extrafont)  ## prettier fonts
library(ggplot2)    ## viz
library(grDevices)  ## anti-aliasing within RStudio
library(lubridate)  ## date wrangling

# ## import calibri (only need to run this one time)
# font_import()

# ## anti-alias (only need to run this once)
# trace(grDevices::png, quote({
#   if (missing(type) && missing(antialias)) {
#     type <- "cairo-png"
#     antialias <- "subpixel"
#   }
# }), print = FALSE)

## load data
d <- fread("https://raw.githubusercontent.com/johnsug/barkley_plots/main/2021_results.csv")

## tag if still active (manual updating)
runners <- unique(d$Runner)
dropped <- runners[1:21] ## update after runners drop out
d[, Active:="Active"]    ## default to "Active"
d[Runner %in% dropped, Active:="Quit"]

## central time offset (5 hours = 300 minutes = 18,000 seconds)
cto <- (5*60*60)

## define datetimes
fmt <- "%Y-%m-%d %H:%M"
barkley_start <- as_datetime("2021-03-18 02:04", format=fmt) ## 3:04 AM ET
cut1 <- barkley_start + 40/3*60*60*1
cut2 <- cut1 + 40/3*60*60*1
cut3 <- cut2 + 40/3*60*60*1 ## 7:04 PM ET
current_time <- as_datetime(Sys.time() - cto)

## date/time formatting
d[, Runner:=factor(Runner, levels=runners)]
d[, Start:=as_datetime(Start, format=fmt)]
d[, End:=as_datetime(End, format=fmt)]
d[is.na(End), End:=current_time] ## if endtime is not given, set to current time

## time remaining
t0 <- as.numeric(cut3 - current_time)
t0 <- t0 * 24
t0_hr <- floor(t0)
t0_min <- floor((t0 - floor(t0))*60)

## render plot
g <- ggplot(d) + 
      
      # cut offs lines
      geom_hline(yintercept=cut1) + 
      geom_hline(yintercept=cut2) + 
      geom_hline(yintercept=cut3) + 
      
      # plot segments (color depends on Active variable)
      geom_segment(aes(x=Runner, xend=Runner, y=Start, yend=End, color=Active), size=2) + 
      
      # completed = end cap
      geom_point(data=d[Complete=="Yes"], aes(x=Runner, y=End, color=Active), size=3) + 
      
      # DNF = open end cap
      geom_point(data=d[Complete=="No"], aes(x=Runner, y=End, color=Active), size=3) +
      geom_point(data=d[Complete=="No"], aes(x=Runner, y=End), color="white", size=1) + 
      
      # # fun run cutoff label
      # #geom_text(data=data.frame(x="Jamil Coury", y=cut3), aes(x=x, y=y, label="Cutoff\n7:04 PM ET", hjust=1.1, vjust=1), size=3) + 
      
      # misc formatting
      ylim(barkley_start, cut3) + 
      scale_color_manual(values=c(Active="dodgerblue", Quit="red")) + 
      labs(x="", y="", title="Barkley 2021 Tracker", 
           #subtitle=paste0(t0_hr, " Hours, ", t0_min, " Minutes Remaining for Fun Run"), 
           caption="Created by John Sugden\nUsing tweets from @keithdunn") + 
      coord_flip() + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, size=14, family="Calibri"), 
            plot.subtitle = element_text(hjust = 0.5, size=10, family="Calibri"), 
            text = element_text(family="Calibri", size=10), 
            legend.position = "none")

## preview
print(g)

## save out
save_file <- paste0("barkley_", gsub(":", "", gsub(" ", "_", gsub("-", "", substr(current_time, 1, 16)))), ".png")
ggsave(g, filename=save_file, dpi=125, type="cairo", width=4, height=4)

