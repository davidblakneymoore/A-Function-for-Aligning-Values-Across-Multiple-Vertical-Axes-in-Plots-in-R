# A Function for Aligning Values Across Multiple Vertical Axes in Plots in R

This repository contains a function for aligning values across multiple vertical axes in plots in R. This function generates new axis limits for all vertical axes axis such that particular values on the various vertical axes are aligned vertically. The user defines these values to align. Though the example at the end of the R script uses `base` R plotting functions, these new axis limits can be used with plotting functions from any package.

This function takes 6 arguments. Only the first is required.

`...` are the numeric variables you wish to appear on the vertical axes.

`Data_Frame` is an optional argument you can provide if all the `...` arguments come from the same data frame and you only want to type the data frame name once.

`Values_to_Align = rep(0, length(list(...)))` are the values you wish to align across the vertical axes. The default for this argument is a vector of `0`s. This argument must be a numeric vector and it must contain the same number of elements that there are variables being aligned.

`Variable_Weights = rep((1 / length(list(...))), length(list(...)))` are the weights assigned to each variable. To prevent certain variables from being crowded near the top or the bottom of the plot, a greater weight can be assigned to these variables, which ensures that these variables will take up more of the plotting region (at the expense of other variables, of course). The default for this argument is to assign all the variables the same weight. This argument must be a numeric vector, all the entries must be finite and nonnegative, and it must contain the same number of elements as there are variables being aligned.

`Upper_Axis_Buffers = rep(0.05, length(list(...)))` are the minimum fractions of blank space you wish to leave around the top of the graph for each variable (above each variable's plotted points). The default for each variable is `0.05` - in other words, at least 5 % of the space on the top of the graph will be empty above each variable's plotted points. This argument must be a numeric vector, all the entries must be between `0` and `1` (inclusive), and it must contain the same number of elements as there are variables being aligned.

`Lower_Axis_Buffers = rep(0.05, length(list(...)))` are the minimum fractions of blank space you wish to leave around the bottom of the graph for each variable (below each variable's plotted points). The default for each variable is `0.05` - in other words, at least 5 % of the space on the bottom of the graph will be empty below each variable's plotted points. This argument must be a numeric vector, all the entries must be between `0` and `1` (inclusive), and it must contain the same number of elements as there are variables being aligned.

This function was inspired by my colleague Sam Zuckerman's need to align `0` values across primary and secondary vertical axes.
