# Script for a longitudinal measurement invariance test in multi-group design
# The following is a description of the longitudinal measurement of the full scale scale of the dimension of origin in multigroup design. 
# The list function (`c ()`)) lists the restrictions. These are gradually equated. For this the `lavaan` package is used [@ Rosseel2012].
# Instead of a measurement invariance check on the formulation of models, the function `measurementInvariance ()` 
# from the package `semTools can be used. Here are the possibilities of investigation at Messvarianz and the model specification rudimentary.
# The panel model is obtained by removing the covariance operator (~~) between the latent constructs by a regression operator (~).
# Finally, the example shows the transformation of the results from the internal consistency analyzes (function: `reliability ()` from `semTools` and` psych`) 
# into an automated table with `knitr` and` kableExtra`.

```{r longMI, echo = TRUE}
# Installl necessary packages
install.packages("lavaan")
install.packages("knitr")
install.packages("semTools")
install.packages("kableExtra")
install.packages("psych")

# Load libraries
library(lavaan)
library(knitr)
library(semTools)
library(kableExtra)
library(psych)

# Configurable measurement invariance check:
# No restrictions on the test form or time of measurement.
# The reliability values are determined at the end.

config <-  '

# Define latent variables with =~. The oberserved variables will follow.
Source1 =~ c(l1a,l1b)*Herkunft_MZP1_1 + c(l2a,l2b)*Herkunft_MZP1_2 + 
c(l3a,l3b)*Herkunft_MZP1_3 + c(l4a,l4b)*Herkunft_MZP1_4 + 
c(l5a,l5b)*Herkunft_MZP1_5
Source2 =~ c(l1c,l1d)*Herkunft_MZP2_1 + c(l2c,l2d)*Herkunft_MZP2_2 + 
c(l3c,l3d)*Herkunft_MZP2_3 + c(l4c,l4d)*Herkunft_MZP2_4 + 
c(l5c,l5d)*Herkunft_MZP2_5

# Effects Coded Method according to Little (2013). Empirical distribution
# of the observed variance on the number of manifest variables on the
# Restrictions. Alternative to the speaker approach.

l1a == 5-l2a-l3a-l4a-l5a
l1b == 5-l2b-l3b-l4b-l5b
l1a == 5-l2c-l3c-l4c-l5c
l1b == 5-l2d-l3d-l4d-l5d

# Latent variances
Source1~~Source1
Source2~~Source2

# Latent means
Source1~1
Source2~1

# Latente covariances
Source2~~Source1

# Manifest variances at measurement point 1 (= MZP1)
Herkunft_MZP1_1~~Herkunft_MZP1_1
Herkunft_MZP1_2~~Herkunft_MZP1_2
Herkunft_MZP1_3~~Herkunft_MZP1_3
Herkunft_MZP1_4~~Herkunft_MZP1_4
Herkunft_MZP1_5~~Herkunft_MZP1_5

# Manifest variances at measurement point 2 (= MZP2)
Herkunft_MZP2_1~~Herkunft_MZP2_1
Herkunft_MZP2_2~~Herkunft_MZP2_2
Herkunft_MZP2_3~~Herkunft_MZP2_3
Herkunft_MZP2_4~~Herkunft_MZP2_4
Herkunft_MZP2_5~~Herkunft_MZP2_5

# Manifest covariances over both measurement points
Herkunft_MZP2_1~~Herkunft_MZP1_1
Herkunft_MZP2_2~~Herkunft_MZP1_2
Herkunft_MZP2_3~~Herkunft_MZP1_3
Herkunft_MZP2_4~~Herkunft_MZP1_4
Herkunft_MZP2_5~~Herkunft_MZP1_5

# Manifest means at measurement point 1
Herkunft_MZP1_1~c(r1a,r1b)*1
Herkunft_MZP1_2~c(r2a,r2b)*1
Herkunft_MZP1_3~c(r3a,r3b)*1
Herkunft_MZP1_4~c(r4a,r4b)*1
Herkunft_MZP1_5~c(r5a,r5b)*1

# Manifest means at measurement point 2
Herkunft_MZP2_1~c(r1c,r1d)*1
Herkunft_MZP2_2~c(r2c,r2d)*1
Herkunft_MZP2_3~c(r3c,r3d)*1
Herkunft_MZP2_4~c(r4c,r4d)*1
Herkunft_MZP2_5~c(r5c,r5d)*1

# Effects Coded Method according to Little (2013).
# So far, no tau equivalency has been established over 
# both measurement points.
r1a==0-r2a-r3a-r4a-r5a
r1b==0-r2b-r3b-r4b-r5b
r1c==0-r2c-r3c-r4c-r5c
r1d==0-r2d-r3d-r4d-r5d
'

# Fit the model
fit.configMI <- lavaan(model = config,data = DATA, 
                                group = "GROUP", 
                                estimator = "mlr", 
                                missing = "FIML")

# Get summary statistics
summary(fit.metrischMI, standardized=TRUE, 
        fit.measures=TRUE, rsquare = TRUE)

# To determine the reliabilities, the script creates a list.
# The reliability () function returns Cronbach's alpha,
# McDonald's omega and the AVE.
rel.list <- list()
rel.list$fit.konfigMI <- reliability(fit.configMI)

# Metric measurement invariance check: Limitations of factor loadings
# about the group factor and the measurement points. In the example, 
# the constraint terms "l1a", "l1b", "l1c", "l1d" changes to to "l1" and so on.

metric <-  '

Source1 =~ c(l1,l1)*Herkunft_MZP1_1 + c(l2,l2)*Herkunft_MZP1_2 + 
c(l3,l3)*Herkunft_MZP1_3 + c(l4,l4)*Herkunft_MZP1_4 + 
c(l5,l5)*Herkunft_MZP1_5
Source2 =~ c(l1,l1)*Herkunft_MZP2_1 + c(l2,l2)*Herkunft_MZP2_2 + 
c(l3,l3)*Herkunft_MZP2_3 + c(l4,l4)*Herkunft_MZP2_4 + 
c(l5,l5)*Herkunft_MZP2_5

## Effects-Coded method accoriding to Little (2013).
l1 == 5-l2-l3-l4-l5

Source1~~Source1
Source2~~Source2

Source1~1
Source2~1

Source2~~Source1

Herkunft_MZP1_1~~Herkunft_MZP1_1
Herkunft_MZP1_2~~Herkunft_MZP1_2
Herkunft_MZP1_3~~Herkunft_MZP1_3
Herkunft_MZP1_4~~Herkunft_MZP1_4
Herkunft_MZP1_5~~Herkunft_MZP1_5

Herkunft_MZP2_1~~Herkunft_MZP2_1
Herkunft_MZP2_2~~Herkunft_MZP2_2
Herkunft_MZP2_3~~Herkunft_MZP2_3
Herkunft_MZP2_4~~Herkunft_MZP2_4
Herkunft_MZP2_5~~Herkunft_MZP2_5

Herkunft_MZP2_1~~Herkunft_MZP1_1
Herkunft_MZP2_2~~Herkunft_MZP1_2
Herkunft_MZP2_3~~Herkunft_MZP1_3
Herkunft_MZP2_4~~Herkunft_MZP1_4
Herkunft_MZP2_5~~Herkunft_MZP1_5

Herkunft_MZP1_1~c(r1a,r1b)*1
Herkunft_MZP1_2~c(r2a,r2b)*1
Herkunft_MZP1_3~c(r3a,r3b)*1
Herkunft_MZP1_4~c(r4a,r4b)*1
Herkunft_MZP1_5~c(r5a,r5b)*1

Herkunft_MZP2_1~c(r1c,r1d)*1
Herkunft_MZP2_2~c(r2c,r2d)*1
Herkunft_MZP2_3~c(r3c,r3d)*1
Herkunft_MZP2_4~c(r4c,r4d)*1
Herkunft_MZP2_5~c(r5c,r5d)*1

r1a==0-r2a-r3a-r4a-r5a
r1b==0-r2b-r3b-r4b-r5b
r1c==0-r2c-r3c-r4c-r5c
r1d==0-r2d-r3d-r4d-r5d
'

fit.metricMI <- lavaan(model = metric,data = DATA, 
                                  group = "GROUP", 
                                  estimator = "mlr",
                                  missing = "FIML")
summary(fit.metricMI, standardized=TRUE, 
        fit.measures=TRUE, rsquare = TRUE)

rel.list$fit.metricMI <- reliability(fit.metricMI)

# Scalar measurement invariance check: Limitations of factor loadings
# about the group factor and the measurment points as well as for the mean values
# of the manifest variables. In the example,
# constraint terms "r1a", "r1b", "r1c", "r1d" changes to "r1" and so on.

scalar <-  '
Source1 =~ c(l1,l1)*Herkunft_MZP1_1 + c(l2,l2)*Herkunft_MZP1_2 + 
c(l3,l3)*Herkunft_MZP1_3 + c(l4,l4)*Herkunft_MZP1_4 + 
c(l5,l5)*Herkunft_MZP1_5
Source2 =~ c(l1,l1)*Herkunft_MZP2_1 + c(l2,l2)*Herkunft_MZP2_2 + 
c(l3,l3)*Herkunft_MZP2_3 + c(l4,l4)*Herkunft_MZP2_4 + 
c(l5,l5)*Herkunft_MZP2_5

l1 == 5-l2-l3-l4-l5

Source1~~Source1
Source2~~Source2

Source1~1
Source2~1

Source2~~Source1

Herkunft_MZP1_1~~Herkunft_MZP1_1
Herkunft_MZP1_2~~Herkunft_MZP1_2
Herkunft_MZP1_3~~Herkunft_MZP1_3
Herkunft_MZP1_4~~Herkunft_MZP1_4
Herkunft_MZP1_5~~Herkunft_MZP1_5

Herkunft_MZP2_1~~Herkunft_MZP2_1
Herkunft_MZP2_2~~Herkunft_MZP2_2
Herkunft_MZP2_3~~Herkunft_MZP2_3
Herkunft_MZP2_4~~Herkunft_MZP2_4
Herkunft_MZP2_5~~Herkunft_MZP2_5

Herkunft_MZP2_1~~Herkunft_MZP1_1
Herkunft_MZP2_2~~Herkunft_MZP1_2
Herkunft_MZP2_3~~Herkunft_MZP1_3
Herkunft_MZP2_4~~Herkunft_MZP1_4
Herkunft_MZP2_5~~Herkunft_MZP1_5

# Tau equivalency established over both measurement points.
Herkunft_MZP1_1~c(r1,r1)*1
Herkunft_MZP1_2~c(r2,r2)*1
Herkunft_MZP1_3~c(r3,r3)*1
Herkunft_MZP1_4~c(r4,r4)*1
Herkunft_MZP1_5~c(r5,r5)*1

Herkunft_MZP2_1~c(r1,r1)*1
Herkunft_MZP2_2~c(r2,r2)*1
Herkunft_MZP2_3~c(r3,r3)*1
Herkunft_MZP2_4~c(r4,r4)*1
Herkunft_MZP2_5~c(r5,r5)*1

r1==0-r2-r3-r4-r5
'

fit.scalarMI <- lavaan(model = scalar,data = DATA, 
                                group = "GROUP", 
                                estimator = "mlr")
summary(fit.skalarMI, standardized=TRUE,
        fit.measures=TRUE, rsquare = TRUE)

rel.list$fit.skalarMI <- reliability(fit.skalarMI)

# Strict measurement invariance check: Limitations of factor loadings
# about the test form and the time of measurement, for the averages of the
# manifest variables and their variances at both measurement times
# but not over both measurement times. In the example, the
# constraints v1 and so on are introduced.

strict <-  '
Source1 =~ c(l1,l1)*Herkunft_MZP1_1 + c(l2,l2)*Herkunft_MZP1_2 + 
c(l3,l3)*Herkunft_MZP1_3 + c(l4,l4)*Herkunft_MZP1_4 + 
c(l5,l5)*Herkunft_MZP1_5
Source2 =~ c(l1,l1)*Herkunft_MZP2_1 + c(l2,l2)*Herkunft_MZP2_2 + 
c(l3,l3)*Herkunft_MZP2_3 + c(l4,l4)*Herkunft_MZP2_4 + 
c(l5,l5)*Herkunft_MZP2_5

l1 == 5-l2-l3-l4-l5

Source1~~Source1
Source2~~Source2

Source1~1
Source2~1

Source2~~Source1

# Constarints for the manifest variances at measurement point 1
Herkunft_MZP1_1~~c(v1,v1)*Herkunft_MZP1_1
Herkunft_MZP1_2~~c(v2,v2)*Herkunft_MZP1_2
Herkunft_MZP1_3~~c(v3,v3)*Herkunft_MZP1_3
Herkunft_MZP1_4~~c(v4,v4)*Herkunft_MZP1_4
Herkunft_MZP1_5~~c(v5,v5)*Herkunft_MZP1_5

# Constarints for the manifest variances at measurement point 2
Herkunft_MZP2_1~~c(v1,v1)*Herkunft_MZP2_1
Herkunft_MZP2_2~~c(v2,v2)*Herkunft_MZP2_2
Herkunft_MZP2_3~~c(v3,v3)*Herkunft_MZP2_3
Herkunft_MZP2_4~~c(v4,v4)*Herkunft_MZP2_4
Herkunft_MZP2_5~~c(v5,v5)*Herkunft_MZP2_5

Herkunft_MZP2_1~~Herkunft_MZP1_1
Herkunft_MZP2_2~~Herkunft_MZP1_2
Herkunft_MZP2_3~~Herkunft_MZP1_3
Herkunft_MZP2_4~~Herkunft_MZP1_4
Herkunft_MZP2_5~~Herkunft_MZP1_5

Herkunft_MZP1_1~c(r1,r1)*1
Herkunft_MZP1_2~c(r2,r2)*1
Herkunft_MZP1_3~c(r3,r3)*1
Herkunft_MZP1_4~c(r4,r4)*1
Herkunft_MZP1_5~c(r5,r5)*1

Herkunft_MZP2_1~c(r1,r1)*1
Herkunft_MZP2_2~c(r2,r2)*1
Herkunft_MZP2_3~c(r3,r3)*1
Herkunft_MZP2_4~c(r4,r4)*1
Herkunft_MZP2_5~c(r5,r5)*1

r1==0-r2-r3-r4-r5
'

fit.strictMI <- lavaan(model = strict,data = DATA, 
                                group = "GROUP", 
                                estimator = "mlr",
                                missing = "FIML")
summary(fit.strictMI, standardized=TRUE, 
        fit.measures=TRUE, rsquare = TRUE)


rel.list$fit.striktMI <- reliability(fit.striktMI)

# In the following, the list elements are called and set.
# The rbind () function binds rows together.

df <- rbind(rel.list$fit.konfigMI$GROUP1,
            rel.list$fit.metrischMI$GROUP1,
            rel.list$fit.skalarMI$GROUP1,
            rel.list$fit.striktMI$GROUP1,
            rel.list$fit.konfigMI$GROUP2,
            rel.list$fit.metrischMI$GROUP2,
            rel.list$fit.skalarMI$GROUP2,
            rel.list$fit.striktMI$GROUP2) 

# The script generates a table with the results of the
# reliability() function. The numbers in the cbind() function indicate the lines
# where McDonald omega is placed.

kable(cbind(c("Herkunft","Sicherheit","Entwicklung","Rechtfertigung",
              "Herkunft","Sicherheit","Entwicklung","Rechtfertigung"),
      round(df[c(4,9,14,19,24,29,34,39),],2)), row.names = FALSE,
      format = "latex", 
      col.names = c("NOS-scale","Measurement point 1",
                    "Measurement point 2","Both Measurement points"),
      caption = "McDonalds-$\\omega$", booktabs = TRUE) %>%
  kableExtra::kable_styling("striped") %>%
  group_rows(group_label = "Group 1",1,4) %>%
  group_rows(group_label = "Group 2",5,8)
```