# # First, re-run the model to re-load it
# outLmer <- lmer( response ~ x + (1|group), data = multIntDemo)
#
# # Second, save the model predictions as a column to the original data.frame
# multIntDemo$lmerPredict <- predict(outLmer)
# multIntDemo$lmerPredict
# # Third, plot the original data
# ggmultIntgDemo2 <- ggplot( multIntDemo, aes(x = x, y = response) ) +
#   geom_point(aes(color = group))+
#   theme_minimal() +
#   scale_color_manual(values = c("blue", "black", "red")) +
#   geom_abline(data = multIntDemo,
#               aes(intercept = intercept, slope = slope, color = group))
#
# # Fourth, use the predicted values to plot the new outputs
# ggmultIntgDemo2 +
#   geom_line( data =  multIntDemo,
#              aes(x = x, y = lmerPredict, color = group),
#              linetype = 2)
