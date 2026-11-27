# the six projects the tutorial cross-validates: HOLZ is kept back for the
# final test, as in the script's `df_noholz`
drop_holz <- function(data = altmext) data[data$pid != "holz", ]
