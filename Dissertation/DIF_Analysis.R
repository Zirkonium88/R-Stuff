```{r dif-bsp, echo=TRUE, eval = FALSE}
# Installl necessary packages
install.packages("pairwise")
install.packages("car")
install.packages("knitr")

# Load libraries
library(pairwise)
library(car)
library(knitr)

# Recoding is done with the recode() function; negative pooled item
# is also need to be recoded (5 becomes 0, etc.).
# The lapply() function makes sure, this works for eevry columns.
# In the example the negatively formulated items are in the
# Columns 1-12 and 28-39.

new.df <- data.frame(lapply(X = df[,c(1:12,28:39)], 
                     FUN = function(x) 
                     recode(x,"5=0;4=1;3=2;2=3;1=4")))

# Create four DIF analyzes in a list with the grm() function.
# nsample: XXX is the number of the total sample
# m: number of categories

new.df.list <- list()
new.df.list$sex <-grm(daten=new.df, m=5, 
                      splitcrit = new.df$sex, nsample = XXX)
new.df.list$ses <-grm(daten=new.df, m=5, 
                      splitcrit = new.df$ses, nsample = XXX)
new.df.list$kft <-grm(daten=new.df, m=5, 
                      splitcrit = new.df$kft, nsample = XXX)
new.df.list$sls <-grm(daten=new.df, m=5, 
                      splitcrit = new.df$sls, nsample = XXX)

# In the next step, the script generates four vector graphics (pdf).
# The font is Times New Roman.
# The function par(mfrow = c (2,2)) specifies the arrangement of the graphics
# before (2 columns and 2 lines)
# The dev.off () function executes the write operation. The file
# is stored in the project folder. \n creates a new line in the title.

pdf(file='Beispiel.pdf',family = "Times New Roman")
par(mfrow=c(2,2))
plot(new.df.list$sex), xlab = "Male", ylab = "Female", 
     main = "Sex", itemNames = TRUE,xymin = -2, xymax = 2)
plot(new.df.list$ses, xlab = "lower then cut-off value", 
     ylab = "higher then cut-off value", main = "Reading abillity", 
     itemNames = TRUE,xymin = -2, xymax = 2)
plot(new.df.list$sls, xlab = "lower then cut-off value", 
     ylab = "higher then cut-off value", main = "SES", 
     itemNames = TRUE,xymin = -2, xymax = 2)
plot(new.df.list$kft, xlab = "lower then cut-off value", 
     ylab = "higher then cut-off value", main = "Intelligence", 
     itemNames = TRUE,xymin = -2, xymax = 2)
dev.off()

# The graphic is included with include_graphics()

include_graphics(path = 'Beispiel.pdf',dpi = 300)

```
