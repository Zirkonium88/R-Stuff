```{r latWM, echo=TRUE, eval = FALSE}
# Installl necessary packages
install.packages("lavaan")

# Load libraries
library(lavaan)

GROWTH.Herk <-  '
Source1 =~ c(l1,l1)*Herkunft_MZP1_1 + c(l2,l2)*Herkunft_MZP1_2 + 
c(l3,l3)*Herkunft_MZP1_3 + c(l4,l4)*Herkunft_MZP1_4 + 
c(l5,l5)*Herkunft_MZP1_5
Source2 =~ c(l1,l1)*Herkunft_MZP2_1 + c(l2,l2)*Herkunft_MZP2_2 + 
c(l3,l3)*Herkunft_MZP2_3 + c(l4,l4)*Herkunft_MZP2_4 + 
c(l5,l5)*Herkunft_MZP2_5

l2 == 4-l3-l4-l5

Source1~~psi*Source1
Source2~~psi*Source2

Source1~0*1
Source2~0*1

Source2~~0*Source1

Herkunft_MZP1_2~~Herkunft_MZP1_2
Herkunft_MZP1_3~~Herkunft_MZP1_3
Herkunft_MZP1_4~~Herkunft_MZP1_4
Herkunft_MZP1_5~~Herkunft_MZP1_5

Herkunft_MZP2_2~~Herkunft_MZP2_2
Herkunft_MZP2_3~~Herkunft_MZP2_3
Herkunft_MZP2_4~~Herkunft_MZP2_4
Herkunft_MZP2_5~~Herkunft_MZP2_5

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

r2==0-r3-r4-r5

# Second-Order  model
int =~ 1*Source1+1*Source2
slope =~ 0*Source1+1*Source2

# Latent means second-order factors
int~1
slope~1 

# Latent variances and covariances of the  second-order factors
slope~~slope
int~~int
slope~~int
'

fit.GROWTH.Herkunft <- lavaan(model = GROWTH.Herk, 
                                data = DATA, 
                                group = "GROUP",
                                estimator = "mlr",
                                missing = "FIML")

```
