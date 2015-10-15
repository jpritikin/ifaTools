savedCodingVersion = 1
recodeTable <- structure(list(val = structure(list(type = structure(c(1L, 1L, 
1L, 1L), .Label = "outcomeSet", class = "factor"), name = structure(c(1L, 
1L, 1L, 1L), .Label = "Match", class = "factor"), nameHash = structure(1:4, .Label = c("36ac7994d8083ab4c85ec97b20cd8bc2", 
"74b1458b0256340ec27b7cda9568e9d1", "2c4879061ffc8489bf951f41ca98693f", 
"d3d32efaf3c9286ed8dc6a25e04d5346"), class = "factor"), action = structure(c(1L, 
1L, 1L, 1L), .Label = "recode", class = "factor"), from = structure(1:4, .Label = c("0", 
"1", "2", "3"), class = "factor"), to = structure(1:4, .Label = c("neither", 
"3 only", "4 only", "both correct"), class = "factor")), .Names = c("type", 
"name", "nameHash", "action", "from", "to"), row.names = c(NA, 
4L), class = "data.frame")), .Names = "val")
permuteTable <- structure(list(items = NULL, reversed = NULL, val = structure(list(
    "61d0a7371a3cc7c966cc3e72ed438465" = c(4L, 1L, 2L, 3L)), .Names = "61d0a7371a3cc7c966cc3e72ed438465")), .Names = c("items", 
"reversed", "val"))
itemModel <- structure(list(Match = structure(list(name = "Match", model = "nrm", 
    outcomes = 4L, factors = 1, starting = structure(c(1.52027370166321, 
    1, 0.54241280051019, 0.0308019804880936, 0.135437474243889, 
    -1.27247751675512, 0), .Names = c("a", "alf1", "alf2", "alf3", 
    "gam1", "gam2", "gam3")), labels = c(NA, NA, "eq1", NA, "eq2", 
    NA, NA), free = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE
    ), Ta = "trend", Tc = "trend"), .Names = c("name", "model", 
"outcomes", "factors", "starting", "labels", "free", "Ta", "Tc"
)), freq = structure(list(name = "freq", model = "grm", outcomes = 11L, 
    factors = 1, starting = structure(c(1.29694917926733, 0.835750925088598, 
    0.401156896788971, -0.113906588576644, -0.755230645597681, 
    -0.952469169575064, -1.43498262919901, -1.81375688009637, 
    -2.3214063354399, -2.5941969893919, -2.62325802714603), .Names = c("a", 
    "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10"
    )), labels = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ), free = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
    TRUE, TRUE, TRUE)), .Names = c("name", "model", "outcomes", 
"factors", "starting", "labels", "free")), Identify = structure(list(
    name = "Identify", model = "nrm", outcomes = 4L, factors = 1, 
    starting = structure(c(0.818969752884284, 1, 0.465113554465825, 
    0.211145880522424, 0.377084276400486, -0.873315750530557, 
    0), .Names = c("a", "alf1", "alf2", "alf3", "gam1", "gam2", 
    "gam3")), labels = c(NA, NA, "eq1", NA, "eq2", NA, NA), free = c(TRUE, 
    FALSE, TRUE, TRUE, TRUE, TRUE, FALSE), Ta = "trend", Tc = "trend"), .Names = c("name", 
"model", "outcomes", "factors", "starting", "labels", "free", 
"Ta", "Tc"))), .Names = c("Match", "freq", "Identify"))
bayesianPrior <- structure(list(map = list()), .Names = "map")
input <- structure(list(numFactors = 1, nameOfFactor1 = "math", nameOfFactor2 = "", 
    nameOfFactor3 = "", nameOfFactor4 = "", nameOfFactor5 = ""), .Names = c("numFactors", 
"nameOfFactor1", "nameOfFactor2", "nameOfFactor3", "nameOfFactor4", 
"nameOfFactor5"))
