install.packages("forestplot")
install.packages("data.table")
library (data.table)
library(forestplot)
library(ggplot2)

#reading data
results <-fread ("agesexpval.csv", header = TRUE)

# Map shapes to corresponding ggplot2 shapes
shape_mapping <- c( "Circle" = 21, "Triangle" = 24, "Square" = 22, "Daimond" = 25)

# Create the forest plot using ggplot2
ggplot(results, aes(x = HR, y = prs, xmin = CIneg, xmax = CIpos, color = color)) +
  geom_pointrange(aes(shape = Shape, color = Color), size = 1) +
  scale_shape_manual(values = shape_mapping, labels = c("disease_4", "disease_433", "disease_24", "disease_43")) +
  scale_color_manual(values = c("purple", "darkblue", "darkgreen", "gold"), labels = c("group_4", "group_3", "group_2", "group_1"))+

  theme_classic() +
  labs(title = "Forest Plot of Hazard Ratios",
       x = "",
       y = "",
       shape = "Shape",
       color = "Color") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  
  theme(legend.position = "bottom")


ggsave("plotina.png",width=10, height=10)

# Create the plot
ggplot(results, aes(x = HR, y = Phenotype, xmin = CIneg, xmax = CIpos, color = color)) +
  geom_pointrange() +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_classic() +
  labs(title = " ", x = "Hazard Ratios", y = " ") +
  scale_color_manual(values = c("purple", "darkblue", "darkgreen", "gold"), labels = c("group_4", "group_3", "group_2", "group_1"))









results$pvals =signif(results$pvals, digits = 3)
results$pvals = formatC(results$pvals, format = "e", digits = 2)
results$pvals = gsub("e", "x10^", results$pvals)

getwd()
write.csv(results, file="agesexpval.csv")












results$color <- rep(c("purple", "darkblue", "darkgreen", "gold"),6)

# Prepare data for forest plot
results$Phenotype <- factor(results$Phenotype, levels = unique(results$Phenotype))

# Create the plot
ggplot(results, aes(x = HR, y = Phenotype, xmin = CIneg, xmax = CIpos, color = color)) +
  geom_pointrange() +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_classic() +
  labs(title = " ", x = "Hazard Ratios", y = " ") +
  scale_color_manual(values = c("purple", "darkblue", "darkgreen", "gold"), labels = c("group_4", "group_3", "group_2", "group_1"))




ggsave("bah.png", height=10, width=10)









# Define colors
color_values <- c("purple" = "purple", "darkblue" = "darkblue", "darkgreen" = "darkgreen", "gold" = "gold")

# Create the plot
ggplot(results, aes(x = HR, y = y_position, color = color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CIneg, xmax = CIpos), height = 0.2) +
  scale_color_manual(values = color_values) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_minimal() +
  scale_y_continuous(breaks = 1:6, labels = unique(results$Phenotype)) +
  labs(title = "Forest Plot", x = "Hazard Ratio (HR)", y = "Phenotype")













results$shape <- rep(c("circle", "square", "triangle", "diamond"), 6)
results$color <- rep(c("purple", "darkblue", "darkgreen", "gold"), 6)



# Define shapes and colors
shape_values <- c("circle" = 16, "square" = 15, "triangle" = 17, "diamond" = 18)
color_values <- c("purple" = "purple", "darkblue" = "darkblue", "darkgreen" = "darkgreen", "gold" = "gold")

# Create the plot
ggplot(results, aes(x = HR, y = Phenotype, shape = shape, color = color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CIneg, xmax = CIpos), height = 0.2) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(title = "Forest Plot", x = "Hazard Ratio (HR)", y = "Phenotype")



# Create the plot
ggplot(results, aes(x = HR, y = y_position, shape = shape, color = color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CIneg, xmax = CIpos), height = 0.2) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_minimal() +
  scale_y_continuous(breaks = 1:6, labels = unique(results$Phenotype)) +
  labs(title = "Forest Plot", x = "Hazard Ratio (HR)", y = "Phenotype")

ggplot(results, aes(x = HR, y = Phenotype, shape = shape, color = color)) +
  geom_point(size = 4, position = position_jitter(width = 0.1, height = 0)) +  # Adjust size and add jitter
  geom_errorbarh(aes(xmin = CIneg, xmax = CIpos), height = 0.2) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_blank())







unique_HR <- unique(results$HR)
shape_values <- c("circle" = 16, "square" = 15, "triangle" = 17, "diamond" = 18)
color_values <- c("purple" = "purple", "darkblue" = "darkblue", "darkgreen" = "darkgreen", "gold" = "gold")

# Create a data frame to map each HR to a shape and color
hr_mapping <- data.frame(
  HR = unique_HR,
  Shape = rep(c("circle", "square", "triangle", "diamond"), length.out = length(unique_HR)),
  Color = rep(c("purple", "darkblue", "darkgreen", "gold"), length.out = length(unique_HR))
)

# Merge the mapping back into the original results data frame
results <- merge(results, hr_mapping, by = "HR")

# Create the plot
ggplot(results, aes(x = HR, y = Phenotype, shape = Shape, color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CIneg, xmax = CIpos), height = 0.2) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_blank())




























hr_matrix <- as.matrix(results[, c("HR", "CIpos", "CIneg")])


# Define the colors for the circles and CI lines
colors <- fpColors(box = c("purple", "darkblue", "darkgreen", "gold"))

# Create the forest plot
forestplot(
  labeltext = results$Phenotype,
  mean = hr_matrix[, 1],
  lower = hr_matrix[, 3],
  upper = hr_matrix[, 2],
  col = colors,
  xlab = "Hazard Ratio",
  title = "Forest Plot of Hazard Ratios by Phenotype",
  new_page = TRUE,
  is.summary = rep(FALSE, nrow(results)),
  xticks = seq(0.5, 2.5, 0.5),
  boxsize = 0.2,
  graph.pos = 2,
  lineheight = unit(1, "cm"),
  fn.ci_norm = fpDrawCircleCI,
  zero = 1,
  col.zero = "red",
  lty.zero = 2
)
library(ggplot2)

quartiles <- c("Youngest", "2nd Quartile", "3rd Quartile", "Oldest")
quartile_colors <- c("Youngest" = "purple", "2nd Quartile" = "darkblue", "3rd Quartile" = "darkgreen", "Oldest" = "gold")

ggplot(results, aes(x = Phenotype, y = HR, ymin = CIneg, ymax = CIpos, color = quartiles)) +
  geom_pointrange(size = 0.8, shape = 21, fill = "white") +
  scale_color_manual(values = quartile_colors) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Forest Plot of Hazard Ratios by Phenotype",
    x = "Phenotype",
    y = "Hazard Ratio",
    color = "Age Quartile"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

forestplot(labeltext = results$Phenotype,
           mean = hr_matrix[, 1],
           lower = hr_matrix[, 3],
           upper = hr_matrix[, 2],
           xlab = "Hazard Ratio",
           title = "Forest Plot of Hazard Ratios by Phenotype",
           new_page = TRUE)



# Create the forest plot
forestplot(
  labeltext = list(phenotypes),
  mean = hr_matrix,
  lower = ci_lower_matrix,
  upper = ci_upper_matrix,
  
  xlab = "Hazard Ratio (HR) with 95% CI",
  new_page = TRUE,
  is.summary = rep(FALSE, length(phenotypes)),
  xticks = seq(0.5, 2.5, 0.5),
  boxsize = 0.2,
  graph.pos = 2
)











# We need to create a matrix for the HR values and CIs for each quartile
phenotypes <- list("AllCancers","Appendicitis","Asthma","Atrial_Fibrillation","Breast_Cancer","CHD","Colorectal_Cancer","Epilepsy",
               "Gout","Hip_Osteoarthritis","Knee_Osteoarthritis","MDD","Prostate_Cancer", "T1D"  ,"T2D","ILD",
               "Lung_Cancer", "Rheumatoid_Arthritis")
  
  
  
hr_matrix <- matrix(NA, nrow = length(phenotypes), ncol = length(quartiles))
ci_lower_matrix <- matrix(NA, nrow = length(phenotypes), ncol = length(quartiles))
ci_upper_matrix <- matrix(NA, nrow = length(phenotypes), ncol = length(quartiles))

# Fill the matrices with the corresponding values
for (i in 1:length(phenotypes)) {
  for (j in 1:length(quartiles)) {
    #subset_data <- results[results$Phenotype == phenotypes[i] & quartiles[j], ]
    subset_data <- results[results$Phenotype == phenotypes[i] & results$Quartile == quartiles[j], ]
    hr_matrix[i, j] <- subset_data$HR
    ci_lower_matrix[i, j] <- subset_data$CIneg
    ci_upper_matrix[i, j] <- subset_data$CIpos
  }
}

# Create the forest plot
forestplot(
  labeltext = list(phenotypes),
  mean = hr_matrix,
  lower = ci_lower_matrix,
  upper = ci_upper_matrix,
  col = fpColors(box = c("gold", "darkgreen", "purple", "darkblue")),
  xlab = "Hazard Ratio (HR) with 95% CI",
  new_page = TRUE,
  is.summary = rep(FALSE, length(phenotypes)),
  xticks = seq(0.5, 2.5, 0.5),
  boxsize = 0.2,
  graph.pos = 2
)























tabletext <- cbind(
  c("Phenotype", results$Phenotype),
  c("HR", results$HR),
  c("CIpos", results$CIpos),
  c("CIneg", results$CIneg)
)
# Remove the first row of tabletext
tabletext <- tabletext[-1, ]

hr_values <- cbind(
  results$HR, results$CIneg, results$CIpos
)



# Create the forest plot
forestplot(
  tabletext,
  hr_values,
  new_page = TRUE,
  is.summary = c(TRUE, rep(FALSE, nrow(results))),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue")
)

















# Reshape the data
results_long <- melt(results, id.vars = c("Phenotype"), 
                     measure.vars = c("HR", "CIpos", "CIneg"),
                     variable.name = "Measure", value.name = "Value")


# Create a matrix for the forest plot
hr_values <- matrix(ncol = 3, nrow = nrow(results_long) / 3)
for (i in 1:(nrow(results_long) / 3)) {
  hr_values[i, ] <- results_long$Value[(3 * i - 2):(3 * i)]
}

forestplot(
  tabletext,
  hr_values,
  new_page = TRUE,
  is.summary = c(TRUE, rep(FALSE, nrow(results))),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue")
)


tabletext <- cbind(
  c("Phenotype", rep(results$Phenotype, each = 4)),
  c("HR", results$HR),
  c("CIpos", results$CIpos),
  c("CIneg", results$CIneg)
)



# Adding a Group column to identify the group numbers
results[, Group := rep(paste0("Group ", 1:4), each = 18)]

# Create the table text for the forest plot
results[, Phenotype := rep(unique(prs), each = 4)]

tabletext <- cbind(
  results$Phenotype,
  results$Group,
  sprintf("%.2f (%.2f - %.2f)", results$HR, results$CIneg, results$CIpos)
)


forestplot(
  labeltext = tabletext,
  mean = results$HR,
  lower = results$CIneg,
  upper = results$CIpos,
  is.summary = rep(c(TRUE, FALSE, FALSE, FALSE), times = 18),
  xlab = "Hazard Ratio",
  new_page = TRUE
)

