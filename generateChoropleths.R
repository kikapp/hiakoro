
source("hiakoroFunctions.R")

# a useable shapefiles can be found in this repository
shape_path <- "/states/states_21basic/states.shp" #state boundaries
shape_path <- "/states/UScounties/UScounties.shp" #county boundaries

#load shape file, grab the juicy bits and 
#create an id variable for merging
us50_shp <- readShapePoly(shape_path)
us50_df <- as.data.frame(us50_shp)
us50_df$id <- as.numeric(row.names(us50_df)) + 1

#generate a data frame with coordinates
#required for plotting the map
#then rename the variables 
us50_points <- sp2tmap(us50_shp)
names(us50_points) <- c("id", "x", "y")

#merge juicy bits with map coordinates
us50 <- merge(x = us50_df, y = us50_points, by.x = "id", by.y = "id")

#look at the ugly
ggplot(data = us50, aes(x=x, y=y, group = id)) + geom_polygon(color = "black", fill = "white")

#split out continental US, AK and HI
#we're gonna scale and shift HI and AK individually


cont_us <- us50[us50$STATE_FIPS != "15" & us50$STATE_FIPS != "02", ]
ak <- us50[us50$STATE_FIPS == "02", ]
hi <- us50[us50$STATE_FIPS == "15", ]

#if you want to plot points in HI or AK
#for instance, city locations
#rbind them into ak or hi here

#define scale matrix
#then generate scaled and shifted coordinates
#tweaking x_shift and y_shift controls where the states end up
#tweaking scale_factor controls the extent to which scaling is implemented
#tweaking scale_mat controls which coordinates are stretched in which directions
scale_mat <- matrix(c(1,0,0,1.25), ncol = 2, byrow = T)
ak_scale <- scaleState(ak, scale_mat, scale_factor = 0.35, x_shift = -120, y_shift = 25)
hi_scale <- scaleState(hi, scale_mat, scale_factor = 1.5, x_shift = -107, y_shift = 25)

#merge map data together
all_us <- rbind(cont_us, ak_scale, hi_scale)

#project if you like some curves in your map
proj_type <- "azequalarea"
projected <- mapproject(x = all_us$x, y = all_us$y, projection=proj_type)
all_us$x_proj <- projected[["x"]]
all_us$y_proj <- projected[["y"]]

#here is where you could add a variable to color your choropleth
#factorConvert converts factors 
#it is available here:
# https://raw.githubusercontent.com/kikapp/GeneralRScripts/master/MiscellaneousRFunctions.R
all_us$name_length <-  nchar(factorConvert(all_us$STATE_NAME, "character"))

#plot it
#blankground is available here:
#https://raw.githubusercontent.com/kikapp/GeneralRScripts/master/generalPlotting.R
ggplot(data = all_us, aes(x=x, y=y, group = id, fill = name_length)) + 
  geom_polygon(color = "black") +
  blankground()

#things don't look as nice when projected
ggplot(data = all_us, aes(x=x_proj, y=y_proj, group = id, fill = name_length)) + 
  geom_polygon(color = "black") +
  blankground()
