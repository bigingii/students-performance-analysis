
# PROJECT 01: STUDENT PERFORMANCE ANALYSIS

# Creating our student dataset

students <- data.frame(
  Student = c("Brian", "Grace", "Kevin", "Mary", "David",
              "Sarah", "John", "Peter", "Faith", "James"),
  
  Mathematics = c(78, 90, 56, 85, 67, 92, 73, 60, 88, 75),
  
  English = c(65, 85, 70, 92, 60, 89, 75, 68, 90, 72),
  
  Biology = c(82, 88, 62, 79, 73, 95, 80, 65, 91, 77),
  
  Physics = c(70, 87, 58, 83, 64, 90, 76, 61, 85, 74),
  
  Geography = c(75, 91, 65, 88, 69, 93, 79, 70, 86, 80)
)

# Display the dataset

print(students)

View(students)
head(students)
tail(students)
dim(students)
str(students)

#Calculate the mean
mean(students$Mathematics)

#Calculate the median
median(students$Mathematics)

#Calculate standard deviation
sd(students$Mathematics)

# Calculate means for all subjects

colMeans(students[, 2:6])

# Highest marks in each subject
sapply(students[, 2:6], max)

# Lowest marks in each subject
sapply(students[, 2:6], min)

library(ggplot2)

# Calculate average marks for each subject

averages <- colMeans(students[, 2:6])

# Convert averages into a data frame

subject_averages <- data.frame(
  Subject = names(averages),
  Average = as.numeric(averages)
)

print(subject_averages)

ggplot(subject_averages, aes(x = Subject, y = Average)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Student Performance by Subject",
    x = "Subjects",
    y = "Average Marks"
  )

# Calculate average marks

averages <- colMeans(students[, 2:6])

# Create a new data frame

subject_averages <- data.frame(
  Subject = names(averages),
  Average = as.numeric(averages)
)

# Display the results

print(subject_averages)

# Load ggplot2
library(ggplot2)

# Create a bar chart
ggplot(subject_averages, aes(x = Subject, y = Average)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Student Performance by Subject",
    x = "Subjects",
    y = "Average Marks"
  )

# Subject with the highest average

subject_averages[which.max(subject_averages$Average), ]

# Subject with the lowest average

subject_averages[which.min(subject_averages$Average), ]

ggplot(subject_averages, aes(x = Subject, y = Average, fill = Subject)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Student Performance Analysis",
    subtitle = "Average marks across five subjects",
    x = "Subjects",
    y = "Average Marks"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Correlation between Mathematics and Physics

cor(students$Mathematics, students$Physics)

plot(students$Mathematics,
     students$Physics,
     main = "Mathematics vs Physics Performance",
     xlab = "Mathematics Marks",
     ylab = "Physics Marks",
     col = "blue",
     pch = 19)

model <- lm(Physics ~ Mathematics, data = students)

abline(model, col = "red", lwd = 2)

cor(students[, c("Mathematics", "English",
                 "Biology", "Physics", "Geography")])

multiple_model <- lm(Physics ~ Mathematics + English +
                       Biology + Geography,
                     data = students)
summary(multiple_model)

par(mfrow = c(2, 2))
plot(multiple_model)

cooks.distance(multiple_model)
which(cooks.distance(multiple_model)>1)

anova(model, multiple_model)

new_student <- data.frame(
  Mathematics = 75,
  English = 70,
  Biology = 80,
  Geography = 85
)

predict(multiple_model, newdata = new_student)

predict_physics <- function(math, english, biology, geography) {
  
  student <- data.frame(
    Mathematics = math,
    English = english,
    Biology = biology,
    Geography = geography
  )
  
  predict(multiple_model, newdata = student)
}

new_students <- data.frame(
  Mathematics = c(75, 90, 65, 80),
  English = c(70, 85, 60, 78),
  Biology = c(80, 88, 72, 85),
  Geography = c(85, 92, 68, 90)
)

new_students$Predicted_Physics <- predict(
  multiple_model,
  newdata = new_students
)
