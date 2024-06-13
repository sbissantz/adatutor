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

# Make the outcome a factor and put it on the 1/2 scale (instead of 0/1)
altmext723$replicated <- factor(altmext723$replicated + 1, # rpart consistency
                                labels = c("failure", "success"))

# Sort the data set by eid
altmext <- altmext723[order(altmext723$eid),]

# Add rownames
rownames(altmext) <- paste0(seq(nrow(altmext)))

# Save the data set as R data
usethis::use_data(altmext, overwrite = TRUE)
