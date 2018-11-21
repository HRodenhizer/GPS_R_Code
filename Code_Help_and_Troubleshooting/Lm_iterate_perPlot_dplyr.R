# conduct regression for every chamber
# define function
tempModel3 <- function(df) { fit <- lm(chambT.mean ~ Tair,  data=df)
return(cbind(
  reg.intercept = summary(fit)$coefficients[1,1],
  reg.slope = summary(fit)$coefficients[2,1],
  reg.r2 = summary(fit)$r.squared
))}

# if you want to know how to extract different parameters just run a regression model and 
# look at the various summaries
reg_parameters <- lm(chambT.mean ~ Tair, data=flux.out)
summary(reg_parameters)
# you can play to see how each parameter gets extracted.
summary(reg_parameters)$coefficients[1,1]
summary(reg_parameters)$coefficients[2,1]

summary(reg_parameters)$coefficients[1,4]

# p value of model
anova(reg_parameters)$'Pr(>F)'[1]

# apply function to every fence and plot
coefs_chambT <- ddply(flux.out, .(fence, plot), tempModel3)

# plot the regression between air temp and chamber temp for each fence-plot
plots = function(.d) ggplot(.d, aes(Tair, chambT.mean)) + geom_point() + stat_smooth (method="lm") +
  labs(title=paste("Fence", .d$fence, ",Plot", .d$plot, sep = " "), x="Tair", y="ChambT")
l <- dlply(flux.out, .(fence, plot), plots)
#ml <- do.call(marrangeGrob, c(l, list(nrow=4, ncol=1)))
ml2 <- marrangeGrob(l, nrow=2, ncol=1)

setwd("~/Desktop/SchuurLab/FluxData_Healy/Autochambers_2016/Data_Processing")
# ggsave("Chamber temperature regression, individual.pdf", ml2, width=10, height=12)

# save coefficients
#save(coefs_chambT, file="ChambTfill_Coefs_2016.Rdata")
#write.table (coefs_chambT, file="ChambTfill_Coefs_2016.csv", sep=",", dec=".", col.names=TRUE, row.names=FALSE)
