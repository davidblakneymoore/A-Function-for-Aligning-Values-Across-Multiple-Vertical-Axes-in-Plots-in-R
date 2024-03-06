
# A Function for Aligning Values Across Multiple Vertical Axes

# David Moore
# davidblakneymoore@gmail.com
# March 2024


# The Explanation

# You may want to align a value (such as '0') on primary and secondary (or even
# tertiary or higher-order) vertical axes. Aligning values can't typically be
# done by default and may be difficult to do manually.

# Here is a function for determining how to align values optimally across
# multiple vertical axes such that there isn't unnecessary  additional space on
# the top or the bottom of any graph and such that the values will align
# perfectly across all vertical axes. This function returns the new axis limits
# for all the vertical axes.

# This function should be used when the vertical axes contain variables that
# are in different units.

# This function takes 6 arguments. Only the first is required.

# '...' are the numeric variables you wish to appear on the vertical axes.

# 'Data_Frame' is an optional argument you can provide if all the '...'
# arguments come from the same data frame and you only want to type the data
# frame name once.

# 'Values_to_Align = rep(0, length(list(...)))' are the values you wish to
# align across the vertical axes. The default for this argument is a vector of
# '0's. This argument must be a numeric vector and it must contain the same
# number of elements that there are variables being aligned.

# 'Variable_Weights = rep((1 / length(list(...))), length(list(...)))' are the
# weights assigned to each variable. To prevent certain variables from being
# crowded near the top or the bottom of the plot, a greater weight can be
# assigned to these variables, which ensures that these variables will take up
# more of the plotting region (at the expense of other variables, of course).
# The default for this argument is to assign all the variables the same weight.
# This argument must be a numeric vector, all the entries must be finite and
# nonnegative, and it must contain the same number of elements as there are
# variables being aligned.

# 'Upper_Axis_Buffers = rep(0.05, length(list(...)))' are the minimum fractions
# of blank space you wish to leave around the top of the graph for each
# variable (above each variable's plotted points). The default for each
# variable is '0.05' - in other words, at least 5 % of the space on the top of
# the graph will be empty above each variable's plotted points. This argument
# must be a numeric vector, all the entries must be between '0' and '1'
# (inclusive), and it must contain the same number of elements as there are
# variables being aligned.

# 'Lower_Axis_Buffers = rep(0.05, length(list(...)))' are the minimum fractions
# of blank space you wish to leave around the bottom of the graph for each
# variable (below each variable's plotted points). The default for each
# variable is '0.05' - in other words, at least 5 % of the space on the bottom
# of the graph will be empty below each variable's plotted points. This
# argument must be a numeric vector, all the entries must be between '0' and
# '1' (inclusive), and it must contain the same number of elements as there are
# variables being aligned.


# The Function

Aligning_Multiple_Vertical_Axes_Function <- function (..., Data_Frame, Values_to_Align = rep(0, length(list(...))), Variable_Weights = rep((1 / length(list(...))), length(list(...))), Upper_Axis_Buffers = rep(0.05, length(list(...))), Lower_Axis_Buffers = rep(0.05, length(list(...)))) {
  if (length(list(...)) <= 1) {
    stop ("There must be more than one variable to warrant aligning vertical axes.")
  }
  Variable_Names <- sapply(match.call(expand.dots = FALSE)$..., deparse)
  if (missing(Data_Frame)) {
    Data_Frame <- as.data.frame(list(...))
    colnames(Data_Frame) <- sapply(match.call(expand.dots = FALSE)$..., deparse)
  } else if (!missing(Data_Frame)) {
    if (class(Data_Frame) != "data.frame") {
      stop ("The 'Data_Frame' argument must be of class 'data.frame'.")
    }
    Data_Frame <- Data_Frame[, which(colnames(Data_Frame) %in% sapply(match.call(expand.dots = FALSE)$..., deparse))]
  }
  if (!all(sapply(Data_Frame, is.numeric))) {
    stop ("All variables to align must contain numeric data.")
  }
  if (length(Values_to_Align) != ncol(Data_Frame)) {
    stop ("The 'Values_to_Align' argument must have the same number of elements as there are variables to align across vertical axes.")
  }
  if (!is.numeric(Values_to_Align) | any(!is.finite(Values_to_Align))) {
    stop ("The 'Values_to_Align' argument must be numeric.")
  }
  if (length(Variable_Weights) != length(list(...))) {
    stop ("The 'Variable_Weights' argument must have the same number of elements as there are variables to align across vertical axes.")
  }
  if (!is.numeric(Variable_Weights) | any(Variable_Weights < 0) | any(!is.finite(Variable_Weights))) {
    stop ("The 'Variable_Weights' argument must contain numeric, nonnegative values.")
  }
  if (!is.numeric(Upper_Axis_Buffers) | any(Upper_Axis_Buffers < 0) | any(Upper_Axis_Buffers > 1) | any(!is.finite(Upper_Axis_Buffers))) {
    stop ("The 'Upper_Axis_Buffers' argument must contain numeric values that are between 0 and 1 (inclusive).")
  }
  if (length(Upper_Axis_Buffers) != length(list(...))) {
    stop ("The 'Upper_Axis_Buffers' argument must contain the same number of elements as there are variables to align across vertical axes.")
  }
  if (!is.numeric(Lower_Axis_Buffers) | any(Lower_Axis_Buffers < 0) | any(Lower_Axis_Buffers > 1) | any(!is.finite(Lower_Axis_Buffers))) {
    stop ("The 'Lower_Axis_Buffers' argument must contain numeric values that are between 0 and 1 (inclusive).")
  }
  if (length(Lower_Axis_Buffers) != length(list(...))) {
    stop ("The 'Lower_Axis_Buffers' argument must contain the same number of elements as there are variables to align across vertical axes.")
  }
  Values_to_Align <- as.list(Values_to_Align)
  Variable_Weights <- Variable_Weights / sum(Variable_Weights)
  Variable_Weights <- as.list(Variable_Weights)
  Number_of_Variables <- ncol(Data_Frame)
  Ranges <- lapply(Data_Frame, function (x) {
    c(min(x), max(x))
  })
  Ranges <- mapply(function (x, y) {
    if ((x[1] < y) & (x[2] < y)) {
      x[2] <- y
    } else if ((x[1] > y) & (x[2] > y)) {
      x[1] <- y
    }
    x
  }, x = Ranges, y = Values_to_Align, SIMPLIFY = FALSE)
  Ratios <- mapply(function (v, w) {
    (w - v[1]) / (v[2] - v[1])
  }, v = Ranges, w = Values_to_Align, SIMPLIFY = FALSE)
  Final_Ratio <- sum(mapply(function (a, b) {
    a * b
  }, a = Ratios, b = Variable_Weights))
  New_Ranges <- mapply(function (u, v, w, x) {
    if (x > Final_Ratio) {
      c(v[1], (v[1] + ((w - v[1]) / Final_Ratio)))
    } else if (x == Final_Ratio) {
      c(v[1], v[2])
    } else if (x < Final_Ratio) {
      c((v[2] - ((v[2] - w) / (1 - Final_Ratio))), v[2])
    }
  }, v = Ranges, w = Values_to_Align, x = Ratios, SIMPLIFY = FALSE)
  Final_Ranges <- mapply(function (x, y, z) {
    c((x[1] - (diff(c(x[1], x[2])) * z)), (x[2] + (diff(c(x[1], x[2])) * y)))
  }, x = New_Ranges, y = Upper_Axis_Buffers, z = Lower_Axis_Buffers, SIMPLIFY = FALSE)
  names(Final_Ranges) <- Variable_Names
  Final_Ranges <- lapply(Final_Ranges, function (x) {
    names(x) <- c("Minimum", "Maximum")
    x
  })
  Final_Ranges
}


# An Example

# Let's make up some data.

set.seed(100)
Number_of_Rows <- 100
Index <- seq_len(Number_of_Rows)
Variable_1 <- rnorm(Number_of_Rows, -10, 1)
Variable_2 <- rnorm(Number_of_Rows, 0, 1)
Variable_3 <- rnorm(Number_of_Rows, 10, 1)
Data_Frame <- data.frame(Index = Index, Variable_1 = Variable_1, Variable_2 = Variable_2, Variable_3 = Variable_3)

# Let's say that we want to align the values -2, 0, and 0 across the three
# vertical axes, respectively.

Values_to_Align = c(-2, 0, 0)

# Let's also weigh the first variable more heavily than the other two.

Variable_Weights <- c(0.75, 0.125, 0.125)

# Let's calculate the new axis limits that will allow these values to align.

(Final_Vertical_Axis_Limits <- Aligning_Multiple_Vertical_Axes_Function(Variable_1, Variable_2, Variable_3, Data_Frame = Data_Frame, Values_to_Align = Values_to_Align, Variable_Weights = Variable_Weights))

# Let's look at the output.

# $Variable_1
#    Minimum    Maximum 
# -12.904407   1.010182 
# 
# $Variable_2
#   Minimum   Maximum 
# -9.945447  2.745460 
# 
# $Variable_3
#   Minimum   Maximum 
# -58.37154  16.11357 

# Finally, let's make a plot.

layout(matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5), ncol = 6, byrow = TRUE), heights = c((1 / 3), (2 / 3)))
par(mar = c(6, 5, 5, 3))
plot(Variable_1 ~ Index, data = Data_Frame, xlab = "", ylab = "", main = "Variable 1", pch = 20)
mtext("Index", 1, line = 2.5)
mtext("Variable 1", 2, line = 2.5)
plot(Variable_2 ~ Index, data = Data_Frame, xlab = "", ylab = "", main = "Variable 2", pch = 20, col = 2)
mtext("Index", 1, line = 2.5)
mtext("Variable 2", 2, line = 2.5)
plot(Variable_3 ~ Index, data = Data_Frame, xlab = "", ylab = "", main = "Variable 3", pch = 20, col = 3)
mtext("Index", 1, line = 2.5)
mtext("Variable 3", 2, line = 2.5)
par(mar = c(12, 6, 6, 12))
plot(Variable_1 ~ Index, data = Data_Frame, xlab = "", ylab = "", yaxt = "n", pch = 20, main = "Multiple Vertical Axes With Values Not Aligned")
axis(2, at = pretty(Data_Frame$Variable_1))
mtext("Index", 1, line = 2.5)
mtext("Variable 1", 2, line = 2.5)
par(new = T)
plot(Variable_2 ~ Index, data = Data_Frame, xlab = "", ylab = "", yaxt = "n", col = 2, pch = 20)
axis(4, at = pretty(Data_Frame$Variable_2))
mtext("Variable 2", 4, line = 2.5)
par(new = T)
plot(Variable_3 ~ Index, data = Data_Frame, xlab = "", ylab = "", yaxt = "n", col = 3, pch = 20)
axis(4, at = pretty(Data_Frame$Variable_3), line = 5)
mtext("Variable 3", 4, line = 7.5)
legend("bottom", xpd = TRUE, inset = c(0, -0.3), title = expression(paste(bold("Variable"))), legend = 1:3, col = 1:3, pch = 20, horiz = T)
plot(Variable_1 ~ Index, data = Data_Frame, xlab = "", ylab = "", yaxt = "n", ylim = Final_Vertical_Axis_Limits[[1]], pch = 20, main = "Multiple Vertical Axes With Values Aligned")
axis(2, at = pretty(range(Final_Vertical_Axis_Limits[[1]])))
mtext("Index", 1, line = 2.5)
mtext("Variable 1", 2, line = 2.5)
abline(h = Values_to_Align[1], lty = 2)
par(new = T)
plot(Variable_2 ~ Index, data = Data_Frame, xlab = "", ylab = "", yaxt = "n", ylim = Final_Vertical_Axis_Limits[[2]], col = 2, pch = 20)
axis(4, at = pretty(range(Final_Vertical_Axis_Limits[[2]])))
mtext("Variable 2", 4, line = 2.5)
abline(h = Values_to_Align[2], lty = 2)
par(new = T)
plot(Variable_3 ~ Index, data = Data_Frame, xlab = "", ylab = "", yaxt = "n", ylim = Final_Vertical_Axis_Limits[[3]], col = 3, pch = 20)
axis(4, at = pretty(range(Final_Vertical_Axis_Limits[[3]])), line = 5)
mtext("Variable 3", 4, line = 7.5)
abline(h = Values_to_Align[3], lty = 2)
legend("bottom", xpd = TRUE, inset = c(0, -0.3), title = expression(paste(bold("Variable"))), legend = 1:3, col = 1:3, pch = 20, horiz = T)

# This plot may be viewed in this repository.
