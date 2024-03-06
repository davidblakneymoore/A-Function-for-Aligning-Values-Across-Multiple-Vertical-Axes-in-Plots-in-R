
# A Function for Aligning Values Across Multiple Vertical Axes

# David Moore
# davidblakneymoore@gmail.com
# March 2024


# The Explanation

# You might want to align a value (such as 0) on both the primary and secondary
# vertical axes. Aligning values can't typically be done by default. You could
# manually define axis limits for both axes to align a value, but if you
# manually choose values for these limits, you might end up with more space on
# the top or the bottom of the graph than is desireable, or you may not be able
# to line up the values across both axes perfectly.

# Here is a function for determining how to optimally align a value on both the
# primary and the secondary vertical axes such that there isn't unnecessary 
# additional space on the top or the bottom of any graph and such that the two
# values will perfectly align across both vertical axes. This function returns
# the new axis limits for both the primary and the secondary vertical axes that
# make the two values you wish to align across both vertical axes line up
# perfectly.

# More often than not, you'll probably want to line up the value of 0 across
# both vertical axes, and you may want to preserve the primary vertical axis
# scale and rescale the secondary vertical axis. This function is flexible and
# allows you to align the axes at any value (you can even choose different
# values for both axes) and it allows you to select which axis scale you wish
# to preserve (you could also choose to preserve neither the primary or the
# secondary vertical axis scales and have the algorithm choose the optimal
# solution).

# To align both axes at particular values, a ratio must be calculated for each
# axis. This ratio is the ratio of the distance of the value to align to the
# minimum value of a particular variable to the distance of the maximum value
# to the minimum value of this same variable. This ratio is calculated for both
# axes. Ratios can be greater than 1 or less than 0 if the value you wish to
# align is not within the range of that variable.

# This function takes 6 arguments. The first two are required, and either
# 'Ratio_of_Value_to_Align_to_Vertical_Axis_Range' or 'Axis_Scale_to_Preserve'
# must be provided as well.

# 'Primary_Vertical_Axis_Variable' is a numeric vector - it's the variable you
# wish to appear on the primary vertical axis.

# 'Secondary_Vertical_Axis_Variable' is a numeric vector - it's the variable
# you wish to appear on the secondary vertical axis.

# 'Data_Frame' is an optional argument you can provide if both the
# 'Primary_Vertical_Axis_Variable' and 'Secondary_Vertical_Axis_Variable'
# arguments come from the same data frame and you only want to type the data
# frame name once.

# 'Primary_Vertical_Axis_Value_to_Align = 0' is the value you wish to align on
# the primary vertical axis. The default value for this argument is 0.

# 'Secondary_Vertical_Axis_Value_to_Align = 0' is the value you wish to align
# on the secondary vertical axis. The default value for this argument is 0.

# 'Ratio_of_Value_to_Align_to_Vertical_Axis_Range' sets where on the vertical
# axis the value to align is. For example, the values to align may both be 0,
# and if the 'Ratio_of_Value_to_Align_to_Vertical_Axis_Range' is 0.5, the value
# to align (0) will be exactly halfway up the vertical axis. If the
# 'Ratio_of_Value_to_Align_to_Vertical_Axis_Range' is 0.25, the values to align
# will both be a quarter of the way up both vertical axes from the bottom of
# the plot. If this argument is a number between 0 and 1, the value to align
# will be visible in the plotting region; if this argument is greater than 1 or
# less than 0, the value to align will not appear in the plotting region, but
# it will still be aligned on both vertical axes. This argument is required
# only if the 'Axis_Scale_to_Preserve' argument is not provided.

# 'Axis_Scale_to_Preserve = "Neither"' tells the function if the scale of one
# of the vertical axes should be preserved and not rescaled at all. The three
# possibilities for this argument are 'Primary', 'Secondary', and 'Neither',
# with the latter being the default. This argument could be used if one of the
# vertical axes is much more important than the other. For example, if you wish
# to plot the primary vertical axis variable such that it takes up the entire
# plotting region and is not rescaled by this function, you would set this
# argument to 'Primary', and all of the rescaling would occur on the secondary
# vertical axis. If the 'Ratio_of_Value_to_Align_to_Vertical_Axis_Range'
# argument is not specified, this argument is required. It should be noted that
# there are cases where it would be impossible to preserve one of the vertical
# axis scales - for example, if one vertical axis variable had a maximum value
# of 10 and a value to align of 15, and the other vertical axis variable had a
# minimum value of 20 and a value to align of 15, there be no possible way to
# plot all of the points while aligning both vertical axes at the value 15.

# 'Axis_Buffer = 10' is the minimum total percent of blank space you wish to
# leave around the top and the bottom of the graph (half of this space will
# appear on the top and half will appear on the bottom). Ten percent is the
# default. It is the minimum total percent of blank space around the top and
# the bottom because if rescaling is necessary to align the values across both
# vertical axes, it follows that more than 10 % of blank space will necessarily
# exist on at least one end of one of the vertical axes. To minimize blank
# space, you could preserve the scale of one of the vertical axes by setting
# the 'Axis_Scale_to_Preserve' argument to either 'Primary' or 'Secondary'.


# The Function

Aligning_Multiple_Vertical_Axes_Function <- function (..., Data_Frame, Values_to_Align = rep(0, length(list(...))), Variable_Weights = rep((1 / length(list(...))), length(list(...))), Axis_Buffer = 0.05) {
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
    stop ("All columns of the data frame must contain numeric data.")
  }
  if (length(Values_to_Align) != ncol(Data_Frame)) {
    stop ("The 'Values_to_Align' argument must have the same number of elements as the 'Data_Frame' argument has columns.")
  }
  if (!is.numeric(Values_to_Align)) {
    stop ("The 'Values_to_Align' argument must be numeric.")
  }
  if (length(Variable_Weights) != ncol(Data_Frame)) {
    stop ("The 'Variable_Weights' argument must have the same number of elements as the 'Data_Frame' argument has columns.")
  }
  if (!is.numeric(Variable_Weights) | any(Variable_Weights < 0) | any(Variable_Weights > 1) | (sum(Variable_Weights) != 1)) {
    stop ("The 'Variable_Weights' argument must contain numeric values that are all greater than or equal to 0 and less than or equal to 1, and the values must sum to 1.")
  }
  if (!is.numeric(Axis_Buffer) | (Axis_Buffer < 0) | (Axis_Buffer > 1) | (length(Axis_Buffer) != 1)) {
    stop ("The 'Axis_Buffer' argument must contain a single numeric value that is between 0 and 1 (inclusive).")
  }
  Values_to_Align <- as.list(Values_to_Align)
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
  Final_Ranges <- lapply(New_Ranges, function (x) {
    c((x[1] - (diff(c(x[1], x[2])) * Axis_Buffer)), (x[2] + (diff(c(x[1], x[2])) * Axis_Buffer)))
  })
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
