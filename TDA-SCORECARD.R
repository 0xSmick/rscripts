#install site catalyst package from CRAN

install.packages("RSiteCatalyst")

#Call SCAuth function for API Credentials before usage

SCAuth("Sheldon.Smickley:Ameritrade", "b8fb19a694361bf51942af07bf19167f")

#Run GetToken Count as a check that the credentials are working propertly
GetTokenCount()





