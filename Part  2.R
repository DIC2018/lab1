# library(reshape2)
# library(ggplot2)
# library(gtable)
# library(grid)
# library(extrafont)
# 
# 
# data <- read.xlsx("Book1.xlsx",sheetName = "Sheet1")
# data$Week <- as.factor(data$Week)
# 
# p1 <- ggplot(data=data,aes(x=Week))+
#   geom_bar(aes(y=Total.A),stat="identity",position ="identity",fill='yellow3',color='lightblue4') +
#   geom_bar(aes(y=Total.B),stat="identity",position ="identity",fill='green3',color='lightblue4')
# 
# 
# p2 <- ggplot(data, aes(x=Week, group=1)) + 
#   geom_line(aes(y = Percent.Positive.A, colour = "yellow3")) + 
#   geom_line(aes(y = Percent.Positive.B, colour = "green3")) + 
#   geom_line(aes(y = X..Positive, colour = "black"))
# 
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# 
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
# 
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# 
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# 
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# ggsave("plot.pdf", g, width=5, height=5)




# 
# library(reshape2)
# library(ggplot2)
# 
# dat <- read.xlsx("Book2.xlsx",sheetName = "Sheet1")
# dat$Week <- as.factor(dat$Week)
# dat$Total...Tested <- NULL
# dat$A.unable.to.sub.type. <- NULL
# DF1 <- melt(dat, id.var="Week")
# 
# ggplot(DF1, aes(x = Week, y = value, fill = variable)) + 
#   geom_bar(stat = "identity",position = "stack") +
# scale_fill_manual("legend", values = c("A.H3N2v." = "blue","A..H1N1.pdm09" = "orange", "A.H3." = "red", "A.Subtyping.not.performed." = "yellow3", "B" = "lightblue", "BVIC"="lightgreen", "BYAM"="green3"))
# 



df <- read.csv("state.csv")

act_split <- str_split_fixed(df$ACTIVITY.LEVEL, " ", 2)
df$activity <- act_split[,2]
mapdata <- data.frame(df$STATENAME,df$activity)
mapdata$df.STATENAME <- tolower(mapdata$df.STATENAME)
colnames(mapdata) <- c("state","activity")

GeoStates <- gvisGeoChart(mapdata, "state", "activity",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))






library(ggplot2)
# We load the geospatial data for the states
# (there are more options to the map_data function, 
# if you are intrested in taking a look).
states <- map_data("state")
# Here I'm creating a sample dataset like yours. 
# The dataset will have 2 columns: The region (or state)
# and a number that will represent the value that you
# want to plot (here the value is just the numerical order of the states).
sim_data <- data.frame(region=unique(states$region), Percent.Turnout=match(unique(states$region), unique(states$region)))
# Then we merge our dataset with the geospatial data:
sim_data_geo <- merge(states, sim_data, by="region")
# The following should give us the plot without the numbers: 
qplot(long, lat, data=sim_data_geo, geom="polygon", fill=Percent.Turnout, group=group)


