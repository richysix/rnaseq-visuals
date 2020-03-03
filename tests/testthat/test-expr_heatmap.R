context('expr_heatmap')
library(rnaseqVis)

set.seed(27)
num_rows <- 50
means <- sample(10:100, size = num_rows)
log2fcs <- rnorm(num_rows)
test_data <- matrix(
  t(sapply(seq_len(50), function(x){
    c( rnorm(6, mean = means[x]), rnorm(6, mean = means[x] * 2^log2fcs[x]) ) } )),
  nrow = 50 )
rownames(test_data) <- paste0('gene_', seq_len(num_rows))

test_that("heatmaps", {
  expect_known_output(expr_heatmap(test_data), file = 'default_heatmap')
  expect_known_output(expr_heatmap(test_data, colour_scale = 'plasma'),
                      file = 'heatmap_plasma')
  expect_known_output(expr_heatmap(test_data, row_labels = paste0('A_', 1:50)),
                      file = 'heatmap_row_labels')
  expect_known_output(expr_heatmap(test_data, legend_position = 'right'),
                      file = 'heatmap_legend_right')
  expect_known_output(expr_heatmap(test_data, scale = 'scale'),
                      file = 'heatmap_scaled')
})
