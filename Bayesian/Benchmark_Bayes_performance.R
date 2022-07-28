# benckmark the performance of Bayesian scan statistics on simulated data

# input settings

# needs to define Bay_performance function

library(foreach)

# detemine the regions to be injected with outbreak

test_regions = seq(from = 1, to = num_reg, by =100)

# for each region inject the outbreak to get simulated data

generate_datasets = function(test_regions) {foreach (i = test_regions) %do% generate_data(i)}

simulated_data_sets  = generate_datasets(test_regions)
# benckmark the performaces
# output a list of days to detection, accuracy, sensitivity and PPVS

# better to define this function at the beginning of the detectio code
Bay_performance = function(simulated_data_sets) {}

# output results
results = Bay_performance(simulated_data_sets)