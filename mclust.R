library("mclust")

ACC_GPS_4075_flight<- na.omit(ACC_GPS_4075_flight)
ACC_GPS_4075_clean<- na.omit(ACC_GPS_4075_clean)

clust4075_flight_axes<- Mclust(ACC_GPS_4075_flight[, c("x","y","z")])
clust4075_flight_SD<- Mclust(ACC_GPS_4075_flight[, c("SDX","SDY","SDZ")])
clust4075_clean_axes<- Mclust(ACC_GPS_4075_clean[, c("x","y","z")])
clust4075_clean_SD<- Mclust(ACC_GPS_4075_clean[, c("SDX","SDY","SDZ")])

plot(clust4075_flight_axes)
plot(clust4075_flight_SD)


summary(clust4075_flight) 

plot(clust4075_flight, what = "density", type = "persp") 

