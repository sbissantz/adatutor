######################################
# Altmejd and SSRP Combined Data Set #
######################################

# Load the Altmejd data set
data("altmejd723")

# Load the SSRP data set
data("ssrp723")

# Variables of interest
voi <- c("eid", "pid", "power.o", "effect_size.o", "n.o", "p_value.o",
         "p_value.o", "replicate")

# Bind the data set with the pre-selected variables
altmext723 <- rbind.data.frame(altmejd723[,colnames(altmejd723) %in% voi],
                                ssrp723[,colnames(ssrp723) %in% voi] )

# Make the outcome a factor and put it on the 1/2 scale (instead of 0/1)
altmext723$replicate <- factor(altmext723$replicate + 1, # rpart consistency
                                labels = c("failure", "success"))


# Extract the project prefix (everything before the dot)
prefix <- sub("\\..*", "", altmejd$eid)

# Extract the numeric part (everything after the dot)
suffix <- as.numeric(sub("^[^.]*\\.", "", altmejd$eid))

# Sort the data set by prefix and suffix
altmejd <- altmext723[order(prefix, suffix),]

# Add rownames
rownames(altmejd) <- paste0(seq_len(nrow(altmejd)))

# Save the data set as R data
usethis::use_data(altmejd, overwrite = TRUE)
