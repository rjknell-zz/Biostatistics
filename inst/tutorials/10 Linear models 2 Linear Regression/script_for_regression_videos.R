### Script for linear regression videos

# Load Chang et al data
grass <- read.csv(file.choose())

# Scatterplot plus regression line and residuals
par(mar = c(5,5,2,2))

plot(grass$BNPP_g_m2 ~ grass$Relative_biomass_grasses_percent, 
		 xlab = "Relative biomass of grasses %",
		 ylab = expression("Below ground primary productivity g.m" ^-2),
		 col = "aquamarine4",
		 pch = 16
)

mod1 <- lm(grass$BNPP_g_m2 ~ grass$Relative_biomass_grasses_percent)

# Regression line
abline(lm(grass$BNPP_g_m2 ~ grass$Relative_biomass_grasses_percent), 
			 lwd = 2, 
			 col = "aquamarine4")

# draw in residuals
arrows(x0 = grass$Relative_biomass_grasses_percent,
			 y0 = grass$BNPP_g_m2,
			 x1 = grass$Relative_biomass_grasses_percent,
			 y1 = mod1$fitted.values,
			 length = 0,
			 col = "aquamarine4"
)


# Scatterplot with poor fit line and residuals
plot(grass$BNPP_g_m2 ~ grass$Relative_biomass_grasses_percent, 
		 xlab = "Relative biomass of grasses %",
		 ylab = expression("Below ground primary productivity g.m" ^-2),
		 col = "aquamarine4",
		 pch = 16
)

abline(a = 180, 
			 b = -0.6,
			 lwd = 2,
			 col = "aquamarine4")

arrows(x0 = grass$Relative_biomass_grasses_percent,
			 y0 = grass$BNPP_g_m2,
			 x1 = grass$Relative_biomass_grasses_percent,
			 y1 = 180 - 0.6*grass$Relative_biomass_grasses_percent,
			 length = 0,
			 col = "aquamarine4"
)

# Plot with grand mean and residuals
plot(grass$BNPP_g_m2 ~ grass$rel_biomass_grass, 
		 xlab = "Relative biomass of grasses %",
		 ylab = expression("Below ground primary productivity g.m" ^-2),
		 col = "aquamarine4",
		 pch = 16
)

abline(h = mean(grass$BNPP_g_m2),
			 lwd = 2,
			 col = "aquamarine4")

arrows(x0 = grass$rel_biomass_grass,
			 y0 = grass$BNPP_g_m2,
			 x1 = grass$rel_biomass_grass,
			 y1 = mean(grass$BNPP_g_m2),
			 length = 0,
			 col = "aquamarine4"
)



# Changing some names so that the code is less messy
names(grass)[9] <- "biomass_grass"

productivity <- data.frame(grass$BNPP_g_m2, grass$biomass_grass)
names(productivity) <- c("BNPP_g_m2", "rel_biomass_grass")

# Calculate means etc.
mean(productivity$BNPP_g_m2)
sd(productivity$BNPP_g_m2)

mean(productivity$rel_biomass_grass)
sd(productivity$rel_biomass_grass)

cor(productivity$BNPP_g_m2, 
		productivity$rel_biomass_grass)

# Regression model 
mod1 <- lm(BNPP_g_m2 ~ rel_biomass_grass, 
					 data = productivity)

summary(mod1)$coefficients

# Scatterplot with means of x and y
plot(grass$BNPP_g_m2 ~ grass$biomass_grass, 
		 xlab = "Relative biomass of grasses %",
		 ylab = expression("Below ground primary productivity g.m" ^-2),
		 col = "aquamarine4",
		 pch = 16
)

abline(h = mean(grass$BNPP_g_m2),
			 lwd = 2,
			 col = "aquamarine4")

arrows(x0 = grass$biomass_grass,
			 y0 = grass$BNPP_g_m2,
			 x1 = grass$biomass_grass,
			 y1 = mean(grass$BNPP_g_m2),
			 length = 0,
			 col = "aquamarine4"
)


# Calculate sums of squares
SSTotal <- sum((productivity$BNPP_g_m2 - 
		mean(productivity$BNPP_g_m2))^2)
SSTotal

predicted <- 215 - 1.68 * productivity$rel_biomass_grass

SSError <- sum((productivity$BNPP_g_m2 - 
	  predicted)^2)						 
SSError


SSTotal - SSError
