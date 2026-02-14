# Script to generate synthetic example data for the package
# Run this once to create the .rds files in inst/extdata/

set.seed(42)

n_zones <- 20    # census tracts
m_points <- 8    # polling locations
k_groups <- 3    # age groups

# Travel time matrix: realistic values (1-120 minutes)
example_tt_matrix <- matrix(
  runif(n_zones * m_points, min = 2, max = 120),
  nrow = n_zones, ncol = m_points
)
rownames(example_tt_matrix) <- paste0("tract_", 1:n_zones)
colnames(example_tt_matrix) <- paste0("poll_", 1:m_points)

# Population matrix: census tract population by age group
example_pop_matrix <- matrix(
  rpois(n_zones * k_groups, lambda = 50),
  nrow = n_zones, ncol = k_groups
)
colnames(example_pop_matrix) <- c("pop_18_34", "pop_35_54", "pop_55_69")

# Source matrix: voter counts at polling locations by age group
# Should roughly sum to total population
example_source_matrix <- matrix(
  rpois(m_points * k_groups, lambda = 125),
  nrow = m_points, ncol = k_groups
)
colnames(example_source_matrix) <- c("vot_18_34", "vot_35_54", "vot_55_69")

saveRDS(example_tt_matrix, "inst/extdata/example_tt_matrix.rds")
saveRDS(example_pop_matrix, "inst/extdata/example_pop_matrix.rds")
saveRDS(example_source_matrix, "inst/extdata/example_source_matrix.rds")
