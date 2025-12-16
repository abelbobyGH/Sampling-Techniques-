## STRATIFIED SAMPLE SIZE WITH COST, TIME, BIAS
## Reference style similar to sampling_2.R

# Example subgroup data (replace with your own)
subgroups <- data.frame(
  h      = 1:5,
  Nh     = c(1000, 800, 600, 400, 200),    # population sizes
  Sh     = c(12, 15, 10, 20, 18),          # SD within subgroup
  ch     = c(50, 40, 30, 80, 100),         # cost per unit
  th     = c(0.5, 0.6, 0.4, 1.0, 1.2),     # time per unit (hours for example)
  bias_risk = c("low", "high", "low", "high", "low")  # qualitative bias indicator
)

N  <- sum(subgroups$Nh)
L  <- nrow(subgroups)

# Design choices: margin of error and confidence level for mean
d        <- 1.5          # desired half-width of CI for mean
alpha    <- 0.05
z_alpha  <- qnorm(1 - alpha/2)
V0       <- (d^2) / (z_alpha^2)  # target variance of stratified mean

# Total budget and total time available
C0 <- 200000    # total allowable cost
T0 <- 1500      # total allowable survey hours

# Effective generalized cost (combine cost and time)
a <- 1          # weight on monetary cost
b <- 200        # weight on time (convert hours to "money" units)
subgroups$ceff <- a * subgroups$ch + b * subgroups$th

# Term used in optimum allocation formula with cost
num_term <- sum(subgroups$Nh * subgroups$Sh / sqrt(subgroups$ceff))
den_term <- N^2 * V0 + sum(subgroups$Nh * (subgroups$Sh^2) / subgroups$ceff)

# Required total sample size from variance criterion
n_var <- (num_term^2) / den_term

cat("Total sample size from variance criterion (n_var):", ceiling(n_var), "\n")

# Allocation ignoring bias and feasibility for the moment
subgroups$nh_var <- n_var * (subgroups$Nh * subgroups$Sh / sqrt(subgroups$ceff)) / num_term

# Bias control: enforce minimum sample sizes in high-bias strata
min_n_low  <- 5
min_n_high <- 10

subgroups$n_min <- ifelse(subgroups$bias_risk == "high", min_n_high, min_n_low)

# First round: enforce minimums
subgroups$nh_alloc <- pmax(subgroups$nh_var, subgroups$n_min)

# Check resource use
total_cost <- sum(subgroups$ch * subgroups$nh_alloc)
total_time <- sum(subgroups$th * subgroups$nh_alloc)

cat("Initial allocation summary:\n")
print(subgroups[, c("h", "Nh", "Sh", "ch", "th", "bias_risk", "nh_alloc")])
cat("Total cost:", round(total_cost, 2), "\n")
cat("Total time:", round(total_time, 2), "\n\n")

# If constraints are binding, scale down proportionally while keeping minimums
scale_allocation <- function(df, C0, T0) {
  # fixed part = enforced minimums
  fixed_n  <- df$n_min
  flex_n   <- df$nh_alloc - fixed_n
  flex_n[flex_n < 0] <- 0
  
  # current cost and time
  cost_now <- sum(df$ch * (fixed_n + flex_n))
  time_now <- sum(df$th * (fixed_n + flex_n))
  
  # if both within limits, nothing to do
  if (cost_now <= C0 && time_now <= T0) {
    return(fixed_n + flex_n)
  }
  
  # we need scale factor s in (0,1] so that
  # sum(ch * (fixed_n + s * flex_n)) <= C0
  # sum(th * (fixed_n + s * flex_n)) <= T0
  # Compute s from both constraints and take min
  cost_flex <- sum(df$ch * flex_n)
  time_flex <- sum(df$th * flex_n)
  
  # avoid division by zero
  s_cost <- if (cost_flex > 0) (C0 - sum(df$ch * fixed_n)) / cost_flex else 1
  s_time <- if (time_flex > 0) (T0 - sum(df$th * fixed_n)) / time_flex else 1
  
  s <- min(1, s_cost, s_time)
  s <- max(s, 0)  # not negative
  
  new_n <- fixed_n + s * flex_n
  return(new_n)
}

subgroups$nh_final <- scale_allocation(subgroups, C0 = C0, T0 = T0)

# Round to integers and recheck
subgroups$nh_final <- pmax(subgroups$n_min, round(subgroups$nh_final))

final_cost <- sum(subgroups$ch * subgroups$nh_final)
final_time <- sum(subgroups$th * subgroups$nh_final)
final_n    <- sum(subgroups$nh_final)

cat("Final allocation (respecting cost, time, bias minimums):\n")
print(subgroups[, c("h", "Nh", "Sh", "ch", "th", "bias_risk", "nh_final")])
cat("Total sample size:", final_n, "\n")
cat("Final total cost:", round(final_cost, 2), "\n")
cat("Final total time:", round(final_time, 2), "\n")

## Plot: design of experiment (allocation by subgroup)

op <- par(mar = c(5, 4, 4, 4) + 0.1)
barplot(
  subgroups$nh_final,
  names.arg = subgroups$h,
  xlab = "Subgroup (h)",
  ylab = "Allocated sample size n_h",
  main = "Design of stratified survey by subgroup",
  col  = ifelse(subgroups$bias_risk == "high", "tomato", "skyblue")
)

# Add line for per-unit cost on secondary axis
par(new = TRUE)
plot(
  1:L, subgroups$ch, type = "b", pch = 16, axes = FALSE, xlab = "", ylab = "",
  col = "darkgreen", lty = 2
)
axis(side = 4)
mtext("Cost per unit (c_h)", side = 4, line = 3)
legend(
  "topright",
  legend = c("n_h (high-bias strata in red)", "Cost per unit"),
  fill   = c("tomato", NA),
  border = c("black", NA),
  lty    = c(NA, 2),
  pch    = c(NA, 16),
  col    = c("tomato", "darkgreen"),
  bg     = "white"
)
par(op)

