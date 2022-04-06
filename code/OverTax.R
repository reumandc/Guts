# OverTax
# 
# OverTax searches through the S3 object with all gut abundances and no NAs and returns a new list with taxa names as the list names and inside each list component, a matrix with mean abundances as the first column, and number of taxa in the given gut as the second column.
# 
# OverTax should throw an error is any NAs are input
# 
# Args:
# dat: the S3 object described above
# 
# Outputs:
# A list containing the mean abundances & total # of taxa for each gut that contain the taxa
OverTax<-function(dat){
  
}