# check that both methods return the correct pairs on the running example

x <- read_reeb_graph("files/running_example_reeb_graph.txt")
p <- reeb_graph_pairs(x, method = "single")
p_ <- p[order(p$birth_index, -p$death_index), ]
expect_equal(
  as.matrix(p[, 1:2]),
  cbind(
    birth_value = c( 0, 11, 13,  9, 12,  7,  4,  1),
    death_value = c(15,  3, 14,  5,  8, 10,  6,  2)
  )
)
