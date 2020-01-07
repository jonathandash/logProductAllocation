# Run analysis for FSA Yield Models

#
library(here)

#Define the file name that will be deleted
r1 <- here('out', 'ProdModels.pdf')
r2 <- here('out', 'out.csv')
#Check its existence
if (file.exists(r1)) 
  #Delete file if it exists
  file.remove(r1)

#Check its existence
if (file.exists(r2)) 
  #Delete file if it exists
  file.remove(r2)




# Read and prepare data
source(here('01_Vol_byDClass.R'))

# Modelling
source(here('ModelPreparer_SQL.R'))


