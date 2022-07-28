library(neastbenchmark)
# Loading mtcars data
data("neastdata")

library("writexl")

mat_2[, 300] = mat_2[, 300]/pop
mat_2[, 305] = mat_2[, 305]/pop
mat_2[, 310] = mat_2[, 310]/pop
mat_2[, 315] = mat_2[, 315]/pop
mat_2[, 320] = mat_2[, 320]/pop
mat_2[, 325] = mat_2[, 325]/pop
mat_2[, 330] = mat_2[, 330]/pop

neastdata$d300 = mat_2[, 300]
neastdata$d305 = mat_2[, 305]
neastdata$d310 = mat_2[, 310]
neastdata$d315 = mat_2[, 315]
neastdata$d320 = mat_2[, 320]
neastdata$d325 = mat_2[, 325]
neastdata$d330 = mat_2[, 330]
# Write the first data set in a new workbook
df = data.frame(neastdata)
write_xlsx(df, "C:/Users/ludo/Documents/df.xlsx")

# df_pop = data.frame(pop)
# write_xlsx(df_pop, "C:/Users/ludo/Documents/pop.xlsx")

# df_pop = data.frame(pop)
# write_xlsx(df_pop, "C:/Users/ludo/Documents/pop.xlsx")
