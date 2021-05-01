
# Aligning a Value on Primary and Secondary Y Axes on
# Plots in R

# By David Moore


# You might want to align a value (such as 0) on both the
# primary and secondary y axes. Aligning values can't
# typically be done by default. You could manually
# define axis limits for both axes to align a value, but
# if you manually choose values for these limits, you
# might end up with more space on the top or the bottom
# of the graph than is desireable.

# This code will determine how to optimally align a value
# on both the primary and the secondary y axes such that
# there isn't too much additional space on the top or the
# bottom of any graph.

# At the bottom of this R script, I provide a function
# for calculating rescaled y axis ranges based on a value
# you wish to align across both y axes.


# Generate some made-up data

Practice_Data <- data.frame(Time = 1:100, Primary_Y_Axis_Variable = sin(1:100 / 15) + rnorm(100, 0, 0.25), Secondary_Y_Axis_Variable = sin(1:100 / 15) + rnorm(100, 1, 0.25))


# Graph the data

par(mar = c(10, 5, 5, 5))
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = ""))
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Aligning Primary and Secondary Y Axes")
abline(h = 0, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", axes = F))
axis(4, pretty(Practice_Data$Secondary_Y_Axis_Variable))
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = 0, col = 2, lty = 2)
legend("bottom", legend = c("Primary X Axis Variable", "Secondary X Axis Variable"), pch = 1, col = 1:2, xpd = T, inset = c(0, -0.45))


# On this graph, it might be desireable to line up the two
# y axes such that 0 is at the same height on the graph for
# both of them.

# The code below will line up a value on both axes. This
# value doesn't necessarily have to be 0, and we will
# define the value by defining a 'Value_to_Align' object.

# This code takes care of all possibly situations. First,
# it determines which axis to shift - to align a particular
# value, only one axis can be shifted if all data points
# are still to appear on the graph. Then, it determines
# whether this axis will have to be shifted up or down (or
# neither, if it is already aligned).


# Let's first define the value to align the axes by.

Value_to_Align <- 0


# Let's also calculate axis buffers. These buffers will
# determine the minimum amount of space to leave on the
# top and the bottom of each graph. I arbitrarily
# selected a 10 % buffer so that 5 % of the axis range
# will be left as blank space on each end (the top and
# the bottom) of the graph. We will add this value to
# the maximum finite value to get the upper axis range
# limit for each axis separately and we will subtract
# this value to the minimum finite value to get the
# lower axis range limit for each axis separately.

Minimum_Primary_Y_Axis_Variable_Value <- min(Practice_Data$Primary_Y_Axis_Variable[is.finite(Practice_Data$Primary_Y_Axis_Variable)])
Maximum_Primary_Y_Axis_Variable_Value <- max(Practice_Data$Primary_Y_Axis_Variable[is.finite(Practice_Data$Primary_Y_Axis_Variable)])
Minimum_Secondary_Y_Axis_Variable_Value <- min(Practice_Data$Secondary_Y_Axis_Variable[is.finite(Practice_Data$Secondary_Y_Axis_Variable)])
Maximum_Secondary_Y_Axis_Variable_Value <- max(Practice_Data$Secondary_Y_Axis_Variable[is.finite(Practice_Data$Secondary_Y_Axis_Variable)])
Primary_Y_Axis_Buffer <- ((Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value) * 1.1 - (Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value)) / 2
Secondary_Y_Axis_Buffer <- ((Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value) * 1.1 - (Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value)) / 2


# Now, we'll determine which axis we will shift. Again,
# only one axis can be shifted to align the value we
# chose while ensuring all data points still appear on
# the graph. To figure out which axis to shift, we can
# see which variable already has the value we wish to
# align closer to plot center.

Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range <- (Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value) / (Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value)
Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range <- (Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value) / (Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value)
if (abs(0.5 - Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) > abs(0.5 - Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) {
  Axis_to_Align <- "Primary"
} else if (abs(0.5 - Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) < abs(0.5 - Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) {
  Axis_to_Align <- "Secondary"
} else if (abs(0.5 - Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) == abs(0.5 - Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) {
  Axis_to_Align <- "Neither"
}


# Now that we know which axis's scale will need to be
# altered, we can calculate the new axis limits.
# Essentially, we will alter the axis we need to
# rescale such that the ratio of the value to align
# to the overall axis range for that axis will be the
# same as that ratio for the axis that isn't being
# altered.

if (Axis_to_Align == "Primary") {
  Primary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
    c(((Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range * Maximum_Primary_Y_Axis_Variable_Value - Value_to_Align) / (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range - 1)) - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
  } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
    c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, ((Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range * Minimum_Primary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) + Primary_Y_Axis_Buffer)
  }
  Secondary_Y_Axis_Range <- c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
} else if (Axis_to_Align == "Secondary") {
  Primary_Y_Axis_Range <- c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
  Secondary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
    c(((Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range * Maximum_Secondary_Y_Axis_Variable_Value - Value_to_Align) / (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range - 1) - Secondary_Y_Axis_Buffer), Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
  } else if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primry_Y_Axis_Range) {
    c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, ((Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range * Minimum_Secondary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) + Secondary_Y_Axis_Buffer)
  }
} else if (Axis_to_Align == "Neither") {
  Primary_Y_Axis_Range <- c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
  Secondary_Y_Axis_Range <- c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
}


# We'll need to generate new axis labels too since we've
# rescaled one of the axes.

if (Axis_to_Align == "Primary") {
  Difference <- unique(diff(pretty(Practice_Data$Primary_Y_Axis_Variable)))
  Primary_Y_Axis_Labels <- if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range < Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
    c(pretty(Practice_Data$Primary_Y_Axis_Variable), sapply(seq_len(abs(floor((max(Primary_Y_Axis_Range) - max(pretty(Practice_Data$Primary_Y_Axis_Variable))) / Difference))), function (x) {
      max(pretty(Practice_Data$Primary_Y_Axis_Variable)) + Difference * x
    }))
  } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range < Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
    c(rev(sapply(seq_len(abs(ceiling((min(Primary_Y_Axis_Range) - min(pretty(Practice_Data$Primary_Y_Axis_Variable))) / Difference))), function (x) {
      min(pretty(Practice_Data$Primary_Y_Axis_Variable)) - Difference * x
    })), pretty(Practice_Data$Primary_Y_Axis_Variable))
  }
  Secondary_Y_Axis_Labels <- pretty(Practice_Data$Secondary_Y_Axis_Variable)
} else if (Axis_to_Align == "Secondary") {
  Primary_Y_Axis_Labels <- pretty(Practice_Data$Primary_Y_Axis_Variable)
  Difference <- unique(diff(pretty(Practice_Data$Secondary_Y_Axis_Variable)))
  Secondary_Y_Axis_Labels <- if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range < Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
    c(pretty(Practice_Data$Secondary_Y_Axis_Variable), sapply(seq_len(abs(floor((max(Secondary_Y_Axis_Range) - max(pretty(Practice_Data$Secondary_Y_Axis_Variable))) / Difference))), function (x) {
      max(pretty(Practice_Data$Secondary_Y_Axis_Variable)) + Difference * x
    }))
  } else if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range < Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
    c(rev(sapply(seq_len(abs(ceiling((min(Secondary_Y_Axis_Range) - min(pretty(Practice_Data$Secondary_Y_Axis_Variable))) / Difference))), function (x) {
      min(pretty(Practice_Data$Secondary_Y_Axis_Variable)) - Difference * x
    })), pretty(Practice_Data$Secondary_Y_Axis_Variable))
  }
} else if (Axis_to_Align == "Neither") {
  Primary_Y_Axis_Labels <- pretty(Practice_Data$Primary_Y_Axis_Variable)
  Secondary_Y_Axis_Labels <- pretty(Practice_Data$Secondary_Y_Axis_Variable)
}


# Here is the final product:

par(mar = c(10, 5, 5, 5))
with(Practice_Data, plot(Time, Primary_Y_Axis_Variable, xlab = "", ylab = "", ylim = Primary_Y_Axis_Range))
axis(1, pretty(Practice_Data$Time))
axis(2, Primary_Y_Axis_Labels)
mtext("Time", 1, 2.5)
mtext("Primary Y Axis Variable", 2, 2.5)
title("Aligning Primary and Secondary Y Axes")
abline(h = 0, lty = 2)
par(new = T)
with(Practice_Data, plot(Time, Secondary_Y_Axis_Variable, col = 2, xlab = "", ylab = "", ylim = Secondary_Y_Axis_Range, axes = F))
axis(4, Secondary_Y_Axis_Labels)
mtext("Secondary Y Axis Variable", 4, 2.5)
abline(h = 0, col = 2, lty = 2)
legend("bottom", legend = c("Primary X Axis Variable", "Secondary X Axis Variable"), pch = 1, col = 1:2, xpd = T, inset = c(0, -0.45))


# We can also write a function that returns the new axis
# limits based on the value you wish to align the axes by.

# This function takes 4 arguments:

# 'Primary_Y_Axis_Variable' is the variable you wish to
# appear on the primary y axis

# 'Secondary_Y_Axis_Variable' is the variable you wish to
# appear on the secondary y axis

# 'Value_to_Align' is the value you wish to align across
# both y axes

# 'Axis_Buffer' is the total percent of blank space you
# wish to leave around the top and the bottom of the graph
# (half of this space will appear on the top and half will
# appear on the bottom)

Axis_Limits_for_Primary_and_Secondary_Y_Axes_Aligned_by_a_Value_Function <- function (Primary_Y_Axis_Variable, Secondary_Y_Axis_Variable, Value_to_Align = 0, Axis_Buffer = 10) {
  if (!any(sapply(list(Primary_Y_Axis_Variable, Secondary_Y_Axis_Variable, Value_to_Align, Axis_Buffer), is.numeric))) {
    stop ("'Primary_Y_Axis_Variable' and 'Secondary_Y_Axis_Variable' arguments must be either of class integer or of class numeric")
  }
  if (length(Primary_Y_Axis_Variable) != length(Secondary_Y_Axis_Variable)) {
    stop ("'Primary_Y_Axis_Variable' and 'Secondary_Y_Axis_Variable' arguments must be the same length")
  }
  Axis_Buffer <- 1 + Axis_Buffer / 100
  Minimum_Primary_Y_Axis_Variable_Value <- min(Primary_Y_Axis_Variable[is.finite(Primary_Y_Axis_Variable)])
  Maximum_Primary_Y_Axis_Variable_Value <- max(Primary_Y_Axis_Variable[is.finite(Primary_Y_Axis_Variable)])
  Minimum_Secondary_Y_Axis_Variable_Value <- min(Secondary_Y_Axis_Variable[is.finite(Secondary_Y_Axis_Variable)])
  Maximum_Secondary_Y_Axis_Variable_Value <- max(Secondary_Y_Axis_Variable[is.finite(Secondary_Y_Axis_Variable)])
  Primary_Y_Axis_Buffer <- ((Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value) * Axis_Buffer - (Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value)) / 2
  Secondary_Y_Axis_Buffer <- ((Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value) * Axis_Buffer - (Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value)) / 2
  Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range <- (Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value) / (Maximum_Primary_Y_Axis_Variable_Value - Minimum_Primary_Y_Axis_Variable_Value)
  Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range <- (Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value) / (Maximum_Secondary_Y_Axis_Variable_Value - Minimum_Secondary_Y_Axis_Variable_Value)
  if (abs(0.5 - Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) > abs(0.5 - Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) {
    Axis_to_Align <- "Primary"
  } else if (abs(0.5 - Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) < abs(0.5 - Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) {
    Axis_to_Align <- "Secondary"
  } else if (abs(0.5 - Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) == abs(0.5 - Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range)) {
    Axis_to_Align <- "Neither"
  }
  if (Axis_to_Align == "Primary") {
    Primary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) {
      c(((Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range * Maximum_Primary_Y_Axis_Variable_Value - Value_to_Align) / (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range - 1)) - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
    } else if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
      c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, ((Value_to_Align - Minimum_Primary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range * Minimum_Primary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) + Primary_Y_Axis_Buffer)
    }
    Secondary_Y_Axis_Range <- c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
  } else if (Axis_to_Align == "Secondary") {
    Primary_Y_Axis_Range <- c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
    Secondary_Y_Axis_Range <- if (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range) {
      c(((Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range * Maximum_Secondary_Y_Axis_Variable_Value - Value_to_Align) / (Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range - 1) - Secondary_Y_Axis_Buffer), Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
    } else if (Ratio_of_Value_to_Align_to_Secondary_Y_Axis_Range > Ratio_of_Value_to_Align_to_Primry_Y_Axis_Range) {
      c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, ((Value_to_Align - Minimum_Secondary_Y_Axis_Variable_Value + Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range * Minimum_Secondary_Y_Axis_Variable_Value) / Ratio_of_Value_to_Align_to_Primary_Y_Axis_Range) + Secondary_Y_Axis_Buffer)
    }
  } else if (Axis_to_Align == "Neither") {
    Primary_Y_Axis_Range <- c(Minimum_Primary_Y_Axis_Variable_Value - Primary_Y_Axis_Buffer, Maximum_Primary_Y_Axis_Variable_Value + Primary_Y_Axis_Buffer)
    Secondary_Y_Axis_Range <- c(Minimum_Secondary_Y_Axis_Variable_Value - Secondary_Y_Axis_Buffer, Maximum_Secondary_Y_Axis_Variable_Value + Secondary_Y_Axis_Buffer)
  }
  return (list(Primary_Y_Axis_Range = Primary_Y_Axis_Range, Secondary_Y_Axis_Range = Secondary_Y_Axis_Range))
}

Axis_Limits_for_Primary_and_Secondary_Y_Axes_Aligned_by_a_Value_Function(Practice_Data$Primary_Y_Axis_Variable, Practice_Data$Secondary_Y_Axis_Variable)
