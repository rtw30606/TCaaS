# https://tensorflow.rstudio.com/tutorials/keras/regression

library(tensorflow)
library(keras)
library(ggfortify)
set.seed(1234)
library(recipes)
library(GGally)
library(rsample)
library(skimr)

MW <- read_feather('Tables/MeasureWeather.feather')
MW$C_next <- lead(MW$C,1)
summary(MW)

# Clean data
lapply(MW, function(x) sum(is.na(x)))  |>  str()
MW <- na.omit(MW)
summary(MW)

# sort by datetime and removed datetime and change room to a character
dataset <- MW |>
  arrange(datetime) |>
  mutate(room = as.factor(room)) |>
  select(!datetime) |>
  select(!id) 

room_labels <- as.character(unique(dataset$room))

dataset <- recipe(C_next ~ ., dataset) |>
#  step_num2factor(room, levels = room_labels) |>
  step_dummy(room, one_hot = TRUE) |>
  prep() |>
  bake(new_data = NULL)
glimpse(dataset)

# Split into training and test datasets

split <- initial_split(dataset, 0.8)
train_dataset <- training(split)
test_dataset <- testing(split)

train_dataset |>
  select(C, C_next, V, loss, ppm, bar, day, step, ex_C, ex_humidity) |>
  ggpairs()
skimr::skim(train_dataset)

# Split features from labels
train_features <- train_dataset  |>  
  select(-C_next)
test_features <- test_dataset  |>  
  select(-C_next)

train_labels <- train_dataset |> 
  select(C)
test_labels <- test_dataset |> select(C)

# Normalization

skim <- skimr::skim_with(numeric = skimr::sfl(mean, sd))
train_dataset |>
  select(where(~is.numeric(.x))) |>
  pivot_longer(
    cols = everything(), names_to = "variable", values_to = "values") |>
  group_by(variable) |>
  summarise(mean = mean(values), sd = sd(values))

normalizer <- layer_normalization(axis = -1L)
normalizer  |> 
  adapt(as.matrix(train_features))
print(normalizer$mean)

first <- as.matrix(train_features[1,])
cat('First example:', first)
cat('Normalized:', as.matrix(normalizer(first)))

# Predict C_next as a function of C

C <- matrix(train_features$C)
C_normalizer <- layer_normalization(input_shape = shape(1), axis = NULL)
C_normalizer  |>  
  adapt(C)

C_model <- keras_model_sequential() %>%
  C_normalizer() %>%
  layer_dense(units = 1)

summary(C_model)
predict(C_model, C_next[1:10,])
C_model  |> 
  compile( optimizer = optimizer_adam(learning_rate = 0.1),
  loss = 'mean_absolute_error')

history <- C_model  |> 
  fit(as.matrix(train_features$C),
  as.matrix(train_labels),
  epochs = 100,
  # Suppress logging.
  verbose = 0,
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2)
plot(history)
# save results
test_results <- list()
test_results[["C_model"]] <- C_model %>% evaluate(
  as.matrix(test_features$C),
  as.matrix(test_labels),
  verbose = 0
)

x <- seq(0, 35, length.out = 36)
y <- predict(C_model, x)

ggplot(train_dataset) +
  geom_point(aes(x = C, y = C_next, color = "data")) +
  geom_line(data = data.frame(x, y), aes(x = x, y = y, color = "prediction"))

# Plot with external temperature
ggplot(train_dataset) +
  geom_point(aes(x = C, y = ex_C))

summary(train_dataset$ex_C)
# Linear regression with multiple inputs