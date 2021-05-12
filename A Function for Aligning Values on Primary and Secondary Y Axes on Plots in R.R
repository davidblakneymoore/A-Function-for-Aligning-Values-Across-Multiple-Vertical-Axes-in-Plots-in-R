
# A Function for Aligning Values on Primary and Secondary
# Y Axes on Plots in R

# By David Moore


# You might want to align a value (such as 0) on both the
# primary and secondary y axes. Aligning values can't
# typically be done by default. You could manually
# define axis limits for both axes to align a value, but
# if you manually choose values for these limits, you
# might end up with more space on the top or the bottom
# of the graph than is desireable, or you may not be able
# to line up the values across both axes perfectly.

# Here is a function for determining how to optimally
# align a value on both the primary and the secondary y
# axes such that there isn't unnecessary additional space
# on the top or the bottom of any graph and such that the
# two values will perfectly align across both y axes. This
# function returns the new axis limits for both the
# primary and the secondary y axes that make the two values
# you wish to align across both y axes line up perfectly.

# More often than not, you'll probably want to line up the
# value of 0 across both y axes, and you may want to
# preserve the primary y axis scale and rescale the
# secondary y axis. This function is flexible and allows
# you to align the axes at any value (you can even choose
# different values for both axes) and it allows you to
# select which axis scale you wish to preserve (you could
# also choose to preserve neither the primary or the
# secondary y axis scales and have the algorithm choose the
# optimal solution).

# To align both axes at particular values, a ratio must be
# calculated for each axis. This ratio is the ratio of the
# distance of the value to align to the minimum value of a
# particular variable to the distance of the maximum value
# to the minimum value of this same variable. This ratio is
# calculated for both axes. Ratios can be greater than 1 or
# less than 0 if the value you wish to align is not within
# the range of that variable.

# This function takes 6 arguments. The first two are
# required, and either
# 'Ratio_of_Value_to_Align_to_Y_Axis_Range' or
# 'Axis_Scale_to_Preserve' must be provided as well.

# 'Primary_Y_Axis_Variable' is a numeric vector - it's the
# variable you wish to appear on the primary y axis.

# 'Secondary_Y_Axis_Variable' is a numeric vector - it's
# the variable you wish to appear on the secondary y axis.

# 'Data_Frame' is an optional argument you can provide if
# both the 'Primary_Y_Axis_Variable' and
# 'Secondary_Y_Axis_Variable' arguments come from the
# same data frame and you only want to type the data
# frame name once.

# 'Primary_Y_Axis_Value_to_Align = 0' is the value you
# wish to align on the primary y axis. The default value
# for this argument is 0.

# 'Secondary_Y_Axis_Value_to_Align = 0' is the value you
# wish to align on the secondary y axis. The default value
# for this argument is 0.

# 'Ratio_of_Value_to_Align_to_Y_Axis_Range' sets where on
# the y axis the value to align is. For example, the
# values to align may both be 0, and if the
# 'Ratio_of_Value_to_Align_to_Y_Axis_Range' is 0.5, the
# value to align (0) will be exactly halfway up the y axis.
# If the 'Ratio_of_Value_to_Align_to_Y_Axis_Range' is 0.25,
# the values to align will both be a quarter of the way up
# both y axes from the bottom of the plot. If this argument
# is a number between 0 and 1, the value to align will be
# visible in the plotting region; if this argument is
# greater than 1 or less than 0, the value to align will
# not appear in the plotting region, but it will still be
# aligned on both y axes. This argument is required only if
# the 'Axis_Scale_to_Preserve' argument is not provided.

# 'Axis_Scale_to_Preserve = "Neither"' tells the function
# if the scale of one of the y axes should be preserved
# and not rescaled at all. The three possibilities for this
# argument are 'Primary', 'Secondary', and 'Neither', with
# the latter being the default. This argument could be used
# if one of the y axes is much more important than the
# other. For example, if you wish to plot the primary y
# axis variable such that it takes up the entire plotting
# region and is not rescaled by this function, you would
# set this argument to 'Primary', and all of the rescaling
# would occur on the secondary y axis. If the
# 'Ratio_of_Value_to_Align_to_Y_Axis_Range' argument is
# not specified, this argument is required. It should be
# noted that there are cases where it would be impossible
# to preserve one of the y axis scales - for example, if
# one y axis variable had a maximum value of 10 and a value
# to align of 15, and the other y axis variable had a
# minimum value of 20 and a value to align of 15, there
# be no possible way to plot all of the points while
# aligning both y axes at the value 15.

# 'Axis_Buffer = 10' is the minimum total percent of blank
# space you wish to leave around the top and the bottom of
# the graph (half of this space will appear on the top and
# half will appear on the bottom). Ten percent is the
# default. It is the minimum total percent of blank space
# around the top and the bottom because if rescaling is
# necessary to align the values across both y axes, it
# follows that more than 10 % of blank space will
# necessarily exist on at least one end of one of the y
# axes. To minimize blank space, you could preserve the
# scale of one of the y axes by setting the
# 'Axis_Scale_to_Preserve' argument to either 'Primary'
# or 'Secondary'.

Axis_Limits_for_Primary_and_Secondary_Y_Axes_Aligned_by_a_Value_Function <- function (Primary_Y_Axis_Variable, Secondary_Y_Axis_Variable, Data_Frame, Primary_Y_Axis_Value_to_Align = 0, Secondary_Y_Axis_Value_to_Align = 0, Ratio_of_Value_to_Align_to_Y_Axis_Range = NULL, Axis_Scale_to_Preserve = "Neither", Axis_Buffer = 10) {
  if (!missing(Data_Frame)) {
    Primary_Y_Axis_Variable <- as.numeric(Data_Frame[[deparse(substitute(Primary_Y_Axis_Variable))]])
    Secondary_Y_Axis_Variable <- as.numeric(Data_Frame[[deparse(substitute(Secondary_Y_Axis_Variable))]])
  } else if (missing(Data_Frame)) {
    Primary_Y_Axis_Variable <- as.numeric(Primary_Y_Axis_Variable)
    Secondary_Y_Axis_Variable <- as.numeric(Secondary_Y_Axis_Variable)
  }
  if (!is.numeric(Primary_Y_Axis_Variable)) {
    stop("'Primary_Y_Axis_Variable' argument must be either of class integer or of class numeric")
  }
  if (!is.numeric(Secondary_Y_Axis_Variable)) {
    stop("'Secondary_Y_Axis_Variable' argument must be either of class integer or of class numeric")
  }
  if (!is.null(Ratio_of_Value_to_Align_to_Y_Axis_Range)) {
    if (!is.numeric(Ratio_of_Value_to_Align_to_Y_Axis_Range)) {
      stop("'Ratio_of_Value_to_Align_to_Y_Axis_Range' argument must be either of class integer or of class numeric")
    }
  }
  if (!is.numeric(Primary_Y_Axis_Value_to_Align)) {
    stop("'Primary_Y_Axis_Value_to_Align' argument must be either of class integer or of class numeric")
  }
  if (!is.numeric(Secondary_Y_Axis_Value_to_Align)) {
    stop("'Secondary_Y_Axis_Value_to_Align' argument must be either of class integer or of class numeric")
  }
  if (!is.numeric(Axis_Buffer)) {
    stop("'Axis_Buffer' argument must be either of class integer or of class numeric")
  }
  if (!is.null(Ratio_of_Value_to_Align_to_Y_Axis_Range)) {
    if (Ratio_of_Value_to_Align_to_Y_Axis_Range <= 0 | Ratio_of_Value_to_Align_to_Y_Axis_Range >= 1) {
      warning ("If the 'Ratio_of_Value_to_Align_to_Y_Axis_Range' argument is not a number between 0 and 1, the values to align will be outside of the plotting region and you risk not having all the data points inside the plotting region")
    }
  }
  if (length(Primary_Y_Axis_Variable) != length(Secondary_Y_Axis_Variable)) {
    stop ("'Primary_Y_Axis_Variable' and 'Secondary_Y_Axis_Variable' arguments must be the same length")
  }
  Values_to_Align <- data.frame(Primary_Y_Axis_Value_to_Align, Secondary_Y_Axis_Value_to_Align)
  Axis_Buffer <- 1 + Axis_Buffer / 100
  Minimum_Primary_Y_Axis_Variable_Value <- min(Primary_Y_Axis_Variable[is.finite(Primary_Y_Axis_Variable)])
  Maximum_Primary_Y_Axis_Variable_Value <- max(Primary_Y_Axis_Variable[is.finite(Primary_Y_Axis_Variable)])
  Minimum_Secondary_Y_Axis_Variable_Value <- min(Secondary_Y_Axis_Variable[is.finite(Secondary_Y_Axis_Variable)])
  Maximum_Secondary_Y_Axis_Variable_Value <- max(Secondary_Y_Axis_Variable[is.finite(Secondary_Y_Axis_Variable)])
  Primary_Y_Axis_Buffer <- ((Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value) * Axis_Buffer - (Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value)) / 2
  Secondary_Y_Axis_Buffer <- ((Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value) * Axis_Buffer - (Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value)) / 2
  Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range <- (Primary_Y_Axis_Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value) / (Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value)
  Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range <- (Secondary_Y_Axis_Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value) / (Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value)
  if (!is.null(Ratio_of_Value_to_Align_to_Y_Axis_Range)) {
    if (Axis_Scale_to_Preserve == "Primary" & Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range != Ratio_of_Value_to_Align_to_Y_Axis_Range) {
      stop ("You cannot preserve the primary y axis scale and set the ratio of the value to align to the y axis range to something other than what it is for the primary y axis")
    }
    if (Axis_Scale_to_Preserve == "Secondary" & Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range != Ratio_of_Value_to_Align_to_Y_Axis_Range) {
      stop ("You cannot preserve the secondary y axis scale and set the ratio of the value to align to the y axis range to something other than what it is for the secondary y axis")
    }
  }
  if (Axis_Scale_to_Preserve == "Primary") {
    Primary_Y_Axis_Range <- c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
    Secondary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
      c(((Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range * Maximum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Value_to_Align) / (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range - 1) - Secondary_Y_Axis_Buffer), Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
    } else if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
      c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, ((Secondary_Y_Axis_Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range * Minimum_Secondary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) + Secondary_Y_Axis_Buffer)
    } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range == Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
      c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
    }
  } else if (Axis_Scale_to_Preserve == "Secondary") {
    Primary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
      c(((Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range * Maximum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Value_to_Align) / (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range - 1)) - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
    } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
      c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, ((Primary_Y_Axis_Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range * Minimum_Primary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) + Primary_Y_Axis_Buffer)
    } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range == Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
      c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
    }
    Secondary_Y_Axis_Range <- c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
  } else if (Axis_Scale_to_Preserve == "Neither") {
    if (is.null(Ratio_of_Value_to_Align_to_Y_Axis_Range)) {
      if (mean(c(Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range, Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) > 0 & mean(c(Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range, Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) < 1) {
        Ratio_of_Value_to_Align_to_Y_Axis_Range <- mean(c(Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range, Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range))
      } else if (mean(c(Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range, Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) <= 0 & mean(c(Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range, Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) >= 1) {
        stop ("Please supply a value for the 'Ratio_of_Value_to_Align_to_Y_Axis_Range' argument between 0 and 1 or specify which axis scale to preserve")
      }
    }
    if (!is.null(Ratio_of_Value_to_Align_to_Y_Axis_Range)) {
      Primary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
        c(((Ratio_of_Value_to_Align_to_Y_Axis_Range * Maximum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Value_to_Align) / (Ratio_of_Value_to_Align_to_Y_Axis_Range - 1)) - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
      } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Y_Axis_Range) {
        c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, ((Primary_Y_Axis_Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Y_Axis_Range * Minimum_Primary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Y_Axis_Range) + Primary_Y_Axis_Buffer)
      } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range == Ratio_of_Value_to_Align_to_Y_Axis_Range) {
        c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
      }
      Secondary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
        c(((Ratio_of_Value_to_Align_to_Y_Axis_Range * Maximum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Value_to_Align) / (Ratio_of_Value_to_Align_to_Y_Axis_Range - 1) - Secondary_Y_Axis_Buffer), Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
      } else if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Y_Axis_Range) {
        c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, ((Secondary_Y_Axis_Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Y_Axis_Range * Minimum_Secondary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Y_Axis_Range) + Secondary_Y_Axis_Buffer)
      } else if (Ratio_of_Value_to_Align_to_Y_Axis_Range == Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
        c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
      }
    }
  }
  return (list(Primary_Y_Axis_Range = Primary_Y_Axis_Range, Secondary_Y_Axis_Range = Secondary_Y_Axis_Range, Values_to_Align = Values_to_Align))
}


# Let's use this function on some made-up data.

# First, generate some made-up data.

Practice_Data <- data.frame(Time = 1:100, Primary_Y_Axis_Variable = sin(1:100 / 15) + rnorm(100, 0, 0.25), Secondary_Y_Axis_Variable = cos(1:100 / 15) + rnorm(100, 0.5, 0.25))

# Next, let's run the function to determine what our y axis
# limits will be.

(New_Axis_Limits <- Axis_Limits_for_Primary_and_Secondary_Y_Axes_Aligned_by_a_Value_Function(Primary_Y_Axis_Variable, Secondary_Y_Axis_Variable, Practice_Data, Ratio_of_Value_to_Align_to_Y_Axis_Range = 0.25))

# Finally, we'll graph the data.

dev.off()
layout(matrix(c(1, 1, 2, 2, 3, 3, 3, 3), byrow = T, nrow = 2), heights = c(0.8, 0.2))
par(mar = c(5, 5, 5, 5))
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = ""))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Unaligned Y Axes")
abline(h = New_Axis_Limits$Values_to_Align$Primary_Y_Axis_Value_to_Align, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", axes = F))
axis(4, pretty(Practice_Data$Secondary_Y_Axis_Variable))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = New_Axis_Limits$Values_to_Align$Secondary_Y_Axis_Value_to_Align, col = 2, lty = 2)
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = "", ylim = New_Axis_Limits$Primary_Y_Axis_Range))
axis(1, pretty(Practice_Data$Time))
axis(2, pretty(New_Axis_Limits$Primary_Y_Axis_Range))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Aligned Y Axes")
abline(h = New_Axis_Limits$Values_to_Align$Primary_Y_Axis_Value_to_Align, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", ylim = New_Axis_Limits$Secondary_Y_Axis_Range, axes = F))
axis(4, pretty(New_Axis_Limits$Secondary_Y_Axis_Range))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = New_Axis_Limits$Values_to_Align$Secondary_Y_Axis_Value_to_Align, col = 2, lty = 2)
par(mar = c(1, 1, 1, 1))
plot(0, type = 'n', axes = F, ylab = "", xlab = "")
legend("top", legend = c("Primary X Axis Variable", "Secondary X Axis Variable", "Primary Y Axis Value to Align", "Secondary Y Axis Value to Align"), pch = c(1, 1, NA, NA), lty = c(NA, NA, 2, 2), col = c(1:2, 1:2))

# Here's another example:

Practice_Data <- data.frame(Time = 1:100, Primary_Y_Axis_Variable = sin(1:100 / 15) + rnorm(100, 0, 0.25), Secondary_Y_Axis_Variable = cos(1:100 / 15) + rnorm(100, 0.5, 0.25))
(New_Axis_Limits <- Axis_Limits_for_Primary_and_Secondary_Y_Axes_Aligned_by_a_Value_Function(Primary_Y_Axis_Variable, Secondary_Y_Axis_Variable, Practice_Data, Axis_Scale_to_Preserve = "Primary"))
dev.off()
layout(matrix(c(1, 1, 2, 2, 3, 3, 3, 3), byrow = T, nrow = 2), heights = c(0.8, 0.2))
par(mar = c(5, 5, 5, 5))
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = ""))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Unaligned Y Axes")
abline(h = New_Axis_Limits$Values_to_Align$Primary_Y_Axis_Value_to_Align, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", axes = F))
axis(4, pretty(Practice_Data$Secondary_Y_Axis_Variable))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = New_Axis_Limits$Values_to_Align$Secondary_Y_Axis_Value_to_Align, col = 2, lty = 2)
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = "", ylim = New_Axis_Limits$Primary_Y_Axis_Range))
axis(1, pretty(Practice_Data$Time))
axis(2, pretty(New_Axis_Limits$Primary_Y_Axis_Range))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Aligned Y Axes")
abline(h = New_Axis_Limits$Values_to_Align$Primary_Y_Axis_Value_to_Align, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", ylim = New_Axis_Limits$Secondary_Y_Axis_Range, axes = F))
axis(4, pretty(New_Axis_Limits$Secondary_Y_Axis_Range))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = New_Axis_Limits$Values_to_Align$Secondary_Y_Axis_Value_to_Align, col = 2, lty = 2)
par(mar = c(1, 1, 1, 1))
plot(0, type = 'n', axes = F, ylab = "", xlab = "")
legend("top", legend = c("Primary X Axis Variable", "Secondary X Axis Variable", "Primary Y Axis Value to Align", "Secondary Y Axis Value to Align"), pch = c(1, 1, NA, NA), lty = c(NA, NA, 2, 2), col = c(1:2, 1:2))

# Here's one more example:

Practice_Data <- data.frame(Time = 1:100, Primary_Y_Axis_Variable = sin(1:100 / 15) + rnorm(100, 25, 0.25), Secondary_Y_Axis_Variable = cos(1:100 / 15) + rnorm(100, -15, 0.25))
(New_Axis_Limits <- Axis_Limits_for_Primary_and_Secondary_Y_Axes_Aligned_by_a_Value_Function(Primary_Y_Axis_Variable, Secondary_Y_Axis_Variable, Practice_Data, Primary_Y_Axis_Value_to_Align = 20, Secondary_Y_Axis_Value_to_Align = -10))
dev.off()
layout(matrix(c(1, 1, 2, 2, 3, 3, 3, 3), byrow = T, nrow = 2), heights = c(0.8, 0.2))
par(mar = c(5, 5, 5, 5))
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = ""))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Unaligned Y Axes")
abline(h = New_Axis_Limits$Values_to_Align$Primary_Y_Axis_Value_to_Align, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", axes = F))
axis(4, pretty(Practice_Data$Secondary_Y_Axis_Variable))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = New_Axis_Limits$Values_to_Align$Secondary_Y_Axis_Value_to_Align, col = 2, lty = 2)
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = "", ylim = New_Axis_Limits$Primary_Y_Axis_Range))
axis(1, pretty(Practice_Data$Time))
axis(2, pretty(New_Axis_Limits$Primary_Y_Axis_Range))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Aligned Y Axes")
abline(h = New_Axis_Limits$Values_to_Align$Primary_Y_Axis_Value_to_Align, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", ylim = New_Axis_Limits$Secondary_Y_Axis_Range, axes = F))
axis(4, pretty(New_Axis_Limits$Secondary_Y_Axis_Range))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = New_Axis_Limits$Values_to_Align$Secondary_Y_Axis_Value_to_Align, col = 2, lty = 2)
par(mar = c(1, 1, 1, 1))
plot(0, type = 'n', axes = F, ylab = "", xlab = "")
legend("top", legend = c("Primary X Axis Variable", "Secondary X Axis Variable", "Primary Y Axis Value to Align", "Secondary Y Axis Value to Align"), pch = c(1, 1, NA, NA), lty = c(NA, NA, 2, 2), col = c(1:2, 1:2))
