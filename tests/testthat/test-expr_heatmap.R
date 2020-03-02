context('expr_heatmap')
library(rnaseqVis)

set.seed(27)
num_rows <- 50
data_l <- list(length=num_rows)
log2fc_l <- numeric(length=num_rows)
mean_l <- numeric(length=num_rows)
for (row in seq_len(num_rows)) {
  mean <- sample(10:100, size = 1)
  mean_l[[row]] <- mean
  log2fc <- rnorm(1)
  log2fc_l[[row]] <- log2fc
  data_l[[row]] <- c( rnorm(6, mean = mean),
                      rnorm(6, mean = mean * 2^log2fc) )
}
test_data <- matrix(unlist(data_l),
                    nrow = num_rows, byrow = TRUE)
rownames(test_data) <- paste0('gene_', seq_len(num_rows))

test_that("heatmaps", {
  expect_known_output(expr_heatmap(test_data), file = 'default_heatmap')
  expect_known_output(expr_heatmap(test_data, colour_scale = 'plasma'),
                      file = 'heatmap_plasma')
  expect_known_output(expr_heatmap(test_data, row_labels = paste0('A_', 1:50)),
                      file = 'heatmap_row_labels')
  expect_known_output(expr_heatmap(test_data, legend_position = 'right'),
                      file = 'heatmap_legend_right')
})
