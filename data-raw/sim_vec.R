# Simulate the default vector of 1000 values from a PLB distribution as used for
#  Figs 1 and 2 of MEE paper.

n = 1000                  # sample size
b.known = -2              # known fixed value of b
xmin.known = 1            # known fixed value of xmin
xmax.known = 1000         # known fixed value of xmax
set.seed(42)              # To get the same observations for each run of code.

sim_vec = rPLB(n,
               b = b.known,
               xmin = xmin.known,
               xmax = xmax.known)

usethis::use_data(sim_vec,
                  overwrite = TRUE)
