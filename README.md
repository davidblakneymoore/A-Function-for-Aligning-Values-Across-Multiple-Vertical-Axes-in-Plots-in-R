# A Function for Aligning Values on Primary and Secondary Y Axes on Plots in R

This repository contains a function for aligning values across primary and secondary y axes on a plot in R. This function returns new, rescaled axis limits for both the primary and the secondary y axis such that a particular value on the primary y axis and a particular value on the secondary y axis are aligned vertically. The user defines these two values to align. Though the example at the end of the R script uses base R plotting functions, these new axis limits can be used with plotting functions from any package.

This function takes 6 arguments. The first two are required, and either `Ratio_of_Value_to_Align_to_Y_Axis_Range` or `Axis_Scale_to_Preserve` must be provided as well.

`Primary_Y_Axis_Variable` is a numeric vector - it's the variable you wish to appear on the primary y axis.

`Secondary_Y_Axis_Variable` is a numeric vector - it's the variable you wish to appear on the secondary y axis.

`Data_Frame` is an optional argument you can provide if both the 'Primary_Y_Axis_Variable' and 'Secondary_Y_Axis_Variable' arguments come from the same data frame and you only want to type the data frame name once.

`Primary_Y_Axis_Value_to_Align = 0` is the value you wish to align on the primary y axis. The default value for this argument is 0.

`Secondary_Y_Axis_Value_to_Align = 0` is the value you wish to align on the secondary y axis. The default value for this argument is 0.

`Ratio_of_Value_to_Align_to_Y_Axis_Range` sets where on the y axis the value to align is. For example, the values to align may both be 0, and if the `Ratio_of_Value_to_Align_to_Y_Axis_Range` is 0.5, the value to align (0) will be exactly halfway up the y axis. If the `Ratio_of_Value_to_Align_to_Y_Axis_Range` is 0.25, the values to align will both be a quarter of the way up both y axes from the bottom of the plot. If this argument is a number between 0 and 1, the value to align will be visible in the plotting region; if this argument is greater than 1 or less than 0, the value to align will not appear in the plotting region, but it will still be aligned on both y axes. This argument is required only if the `Axis_Scale_to_Preserve` argument is not provided.

`Axis_Scale_to_Preserve = "Neither"` tells the function if the scale of one of the y axes should be preserved and not rescaled at all. The three possibilities for this argument are `'Primary'`, `'Secondary'`, and `'Neither'`, with the latter being the default. This argument could be used if one of the y axes is much more important than the other. For example, if you wish to plot the primary y axis variable such that it takes up the entire plotting region and is not rescaled by this function, you would set this argument to `'Primary'`, and all of the rescaling would occur on the secondary y axis. If the `Ratio_of_Value_to_Align_to_Y_Axis_Range` argument is not specified, this argument is required. It should be noted that there are cases where it would be impossible to preserve one of the y axis scales - for example, if one y axis variable had a maximum value of 10 and a value to align of 15, and the other y axis variable had a minimum value of 20 and a value to align of 15, there be no possible way to plot all of the points while aligning both y axes at the value 15.

`Axis_Buffer = 10` is the minimum total percent of blank space you wish to leave around the top and the bottom of the graph (half of this space will appear on the top and half will appear on the bottom). Ten percent is the default. It is the minimum total percent of blank space around the top and the bottom because if rescaling is necessary to align the values across both y axes, it follows that more than 10 % of blank space will necessarily exist on at least one end of one of the y axes. To minimize blank space, you could preserve the scale of one of the y axes by setting the `Axis_Scale_to_Preserve` argument to either `'Primary'` or `'Secondary'`.


This function was inspired by my labmate Sam Zuckerman's need to align 0 values across primary and secondary y axes.
