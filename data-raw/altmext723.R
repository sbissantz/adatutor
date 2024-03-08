######################################
# Altmejd and SSRP Combined Data Set #
######################################

# Load the Altmejd data set
data("altmejd723")

# Load the SSRP data set
data("ssrp723")

# Variables of interest
voi <- c("eid", "pid", "power.o", "effect_size.o", "n.o", "p_value.o",
         "log_p.o", "replicated")

# Bind the data set with the pre-selected variables
altmext723 <- rbind.data.frame( altmejd723[,colnames(altmejd723) %in% voi],
                                ssrp723[,colnames(ssrp723) %in% voi] )

# Save the data set as R data
usethis::use_data(altmext723, overwrite = TRUE)
