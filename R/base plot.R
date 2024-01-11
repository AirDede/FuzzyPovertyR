# verma.plot.data = verma.plot.data %>% dplyr::select(-weight) %>%
#   tidyr::pivot_longer(cols = c(mu, Lorenz, WECDF), names_to = "curve", values_to = "value")
# # Assuming verma.plot.data has columns: predicate, curve, value
#
# # Set up the plot
# # Set up the plot
# plot(verma.plot.data$predicate[verma.plot.data$curve == "mu"],
#      verma.plot.data$value[verma.plot.data$curve == "mu"],
#      type = "n", xlab = "Predicate", ylab = expression(mu))
#
# # Add lines
# lines(verma.plot.data[verma.plot.data$curve == "mu", c("predicate", "value")], col = "black", lty = 1)
# lines(verma.plot.data[verma.plot.data$curve == "Lorenz", c("predicate", "value")], col = "black", lty = 2)
# lines(verma.plot.data[verma.plot.data$curve == "WECDF", c("predicate", "value")], col = "black", lty = 3)
#
# # Add areas under the lines
# polygon(c(verma.plot.data$predicate[verma.plot.data$curve == "mu"],
#           rev(verma.plot.data$predicate[verma.plot.data$curve == "mu"])),
#         c(verma.plot.data$value[verma.plot.data$curve == "mu"],
#           rep(0, sum(verma.plot.data$curve == "mu"))),
#         col = alpha("black", 0.1), lty = 1)
#
# polygon(c(verma.plot.data$predicate[verma.plot.data$curve == "Lorenz"],
#           rev(verma.plot.data$predicate[verma.plot.data$curve == "Lorenz"])),
#         c(verma.plot.data$value[verma.plot.data$curve == "Lorenz"],
#           rep(0, sum(verma.plot.data$curve == "Lorenz"))),
#         col = alpha("black", 0.1), lty = 2)
#
# polygon(c(verma.plot.data$predicate[verma.plot.data$curve == "WECDF"],
#           rev(verma.plot.data$predicate[verma.plot.data$curve == "WECDF"])),
#         c(verma.plot.data$value[verma.plot.data$curve == "WECDF"],
#           rep(0, sum(verma.plot.data$curve == "WECDF"))),
#         col = alpha("black", 0.1))
#
# # Add legend
# legend("bottom", legend = c("mu", "Lorenz", "WECDF"), lty = c(1, 2, 3), col = "black", fill = alpha("black", 0.1))
