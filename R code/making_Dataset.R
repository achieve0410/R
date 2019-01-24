

###############################################################################################################


compliance <- rep(c(1:5), each=2500)
acceleration <- rep( rep(c(1:5), each=500), 5)
deceleration <- rep( rep(c(1:5), each=100), 25)
result <- rep(c(1:100), 125)

tmp_data <- data.frame(compliance, acceleration, deceleration, result)

write.csv(tmp_data, file="tmp_data.csv", row.names = FALSE)


###############################################################################################################

virtual <- read.csv("epoch5000_multiLayer_filter5_result.csv")

virtual <- virtual[ , c(1:4)]

write.csv(virtual, file="driving_score_virtual_plus_origin.csv")


###############################################################################################################