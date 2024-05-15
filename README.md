# Load necessary libraries
library(GA)
library(ggplot2)

# Helper function: Convert integer to binary string
int2bin <- function(integer, bits) {
  bin <- paste0(as.integer(intToBits(integer))[1:bits], collapse = "")
  return(bin)
}

# Helper function: Convert binary string to integer
bit2int <- function(bits) {
  return(sum(as.numeric(unlist(strsplit(bits, ""))) * 2^(rev(seq_along(bits)-1))))
}

# Convert bitstring to architecture
bitstring_to_architecture <- function(bitstring) {
  storage <- bit2int(substr(bitstring, 1, 2))
  biometrics <- bit2int(substr(bitstring, 3, 5))
  control_hub <- bit2int(substr(bitstring, 6, 6))
  devices <- bit2int(substr(bitstring, 7, 8))
  machine_learning <- bit2int(substr(bitstring, 9, 10))
  control <- bit2int(substr(bitstring, 11, 13))
  architecture <- list(
    storage = storage,
    biometrics = biometrics,
    control_hub = control_hub,
    devices = devices,
    machine_learning = machine_learning,
    control = control
  )
  return(architecture)
}

# Repair function: Ensure bitstring represents a valid architecture
repair_bits <- function(bitstring) {
  bit_lengths <- c(2, 3, 1, 2, 2, 3)
  max_values <- c(3, 6, 2, 3, 3, 5)
  repaired_bitstring <- ""
  start <- 1
  for (i in seq_along(bit_lengths)) {
    end <- start + bit_lengths[i] - 1
    segment <- substr(bitstring, start, end)
    value <- bit2int(segment)
    if (is.na(value) || value >= max_values[i]) {
      random_value <- sample(0:(max_values[i] - 1), 1)
      repaired_segment <- int2bin(random_value, bit_lengths[i])
    } else {
      repaired_segment <- segment
    }
    repaired_bitstring <- paste0(repaired_bitstring, repaired_segment)
    start <- end + 1
  }
  return(repaired_bitstring)
}

# Assume evaluate_fitness is a predefined function to calculate fitness values
evaluate_fitness <- function(architecture) {
  # Define your fitness calculation logic here
  cost <- runif(1, 100, 1000)  # Example: Generate random cost
  production_time <- runif(1, 10, 100)  # Example: Generate random production time
  response_time <- runif(1, 1, 10)  # Example: Generate random response time
  reliability <- runif(1, 0.8, 1.0)  # Example: Generate random reliability
  detection_rate <- runif(1, 0.7, 1.0)  # Example: Generate random detection rate
  video_quality <- runif(1, 480, 1080)  # Example: Generate random video quality
  return(list(cost = cost, production_time = production_time, response_time = response_time,
              reliability = reliability, detection_rate = detection_rate, video_quality = video_quality))
}

# Fitness function: Calculate fitness values
fitness_function <- function(bitstring) {
  repaired_bitstring <- repair_bits(bitstring)
  architecture <- bitstring_to_architecture(repaired_bitstring)
  fitness_values <- evaluate_fitness(architecture)
  
  # Print debug information
  cat("Evaluating architecture:\n")
  print(architecture)
  cat("Fitness values:\n")
  print(fitness_values)
  
  return(c(fitness_values$cost, -fitness_values$production_time, fitness_values$response_time,
           fitness_values$reliability, fitness_values$detection_rate, fitness_values$video_quality))
}

# Run GA algorithm
ga_result <- ga(type = "binary",
                fitness = fitness_function,
                nBits = 13,
                popSize = 200,  
                maxiter = 500,  
                pmutation = 0.2,  
                seed = 123)

summary(ga_result)
plot(ga_result)

# Check the distribution of fitness values
fitness_values <- ga_result@fitness
print(fitness_values)

# Extract Pareto front data
pareto_front <- ga_result@solution

# Check Pareto front data
str(pareto_front)

# Convert to data frame
pareto_data <- as.data.frame(pareto_front)

# Assume there is only one solution, manually generate some random values for visualization demonstration
if (nrow(pareto_data) == 1) {
  pareto_data <- rbind(pareto_data, pareto_data + runif(1, 0.1, 1))
  colnames(pareto_data) <- c("cost", "production_time", "response_time", "reliability", "detection_rate", "video_quality")
}

# Visualize Pareto front: Cost vs Production Time
ggplot(pareto_data, aes(x = cost, y = production_time)) +
  geom_point() +
  labs(title = "Pareto Front: Cost vs Production Time", x = "Cost", y = "Production Time")

# Visualize Pareto front: Response Time vs Reliability
ggplot(pareto_data, aes(x = response_time, y = reliability)) +
  geom_point() +
  labs(title = "Pareto Front: Response Time vs Reliability", x = "Response Time", y = "Reliability")

#######################################
library(mco)
library(ggplot2)

# Helper function: Convert integer to binary string
int2bin <- function(integer, bits) {
  bin <- paste0(as.integer(intToBits(integer))[1:bits], collapse = "")
  return(bin)
}

# Helper function: Convert binary string to integer
bit2int <- function(bits) {
  return(sum(as.numeric(unlist(strsplit(bits, ""))) * 2^(rev(seq_along(bits) - 1))))
}

# Convert bitstring to architecture
bitstring_to_architecture <- function(bitstring) {
  storage <- bit2int(substr(bitstring, 1, 2))
  biometrics <- bit2int(substr(bitstring, 3, 5))
  control_hub <- bit2int(substr(bitstring, 6, 6))
  devices <- bit2int(substr(bitstring, 7, 8))
  machine_learning <- bit2int(substr(bitstring, 9, 10))
  control <- bit2int(substr(bitstring, 11, 13))
  architecture <- list(
    storage = storage,
    biometrics = biometrics,
    control_hub = control_hub,
    devices = devices,
    machine_learning = machine_learning,
    control = control
  )
  return(architecture)
}

# Repair function: Ensure bitstring represents a valid architecture
repair_bits <- function(bitstring) {
  bit_lengths <- c(2, 3, 1, 2, 2, 3)
  max_values <- c(3, 6, 2, 3, 3, 5)
  repaired_bitstring <- ""
  start <- 1
  for (i in seq_along(bit_lengths)) {
    end <- start + bit_lengths[i] - 1
    segment <- substr(bitstring, start, end)
    value <- bit2int(segment)
    if (is.na(value) || value >= max_values[i]) {
      random_value <- sample(0:(max_values[i] - 1), 1)
      repaired_segment <- int2bin(random_value, bit_lengths[i])
    } else {
      repaired_segment <- segment
    }
    repaired_bitstring <- paste0(repaired_bitstring, repaired_segment)
    start <- end + 1
  }
  return(repaired_bitstring)
}

# Assume evaluate_fitness is a predefined function to calculate fitness values
evaluate_fitness <- function(architecture) {
  # Define your fitness calculation logic here
  cost <- runif(1, 100, 1000)  # Example: Generate random cost
  production_time <- runif(1, 10, 100)  # Example: Generate random production time
  response_time <- runif(1, 1, 10)  # Example: Generate random response time
  reliability <- runif(1, 0.8, 1.0)  # Example: Generate random reliability
  detection_rate <- runif(1, 0.7, 1.0)  # Example: Generate random detection rate
  video_quality <- runif(1, 480, 1080)  # Example: Generate random video quality
  return(c(cost, production_time, response_time, reliability, detection_rate, video_quality))
}

# Fitness function: Calculate fitness values
objective_function <- function(x) {
  bitstring <- paste(as.integer(x), collapse = "")
  repaired_bitstring <- repair_bits(bitstring)
  architecture <- bitstring_to_architecture(repaired_bitstring)
  fitness_values <- evaluate_fitness(architecture)
  return(fitness_values)
}

# Define decision variables and their ranges
nBits <- 13
lower_bounds <- rep(0, nBits)
upper_bounds <- rep(1, nBits)

# Run multi-objective genetic algorithm
result <- nsga2(
  objective_function,
  idim = nBits,
  odim = 6,
  lower.bounds = lower_bounds,
  upper.bounds = upper_bounds,
  popsize = 200,
  generations = 500
)

# Print results
print(result)

# Extract Pareto front data
pareto_front <- result$value

# Convert to data frame
pareto_data <- as.data.frame(pareto_front)

# Rename columns
colnames(pareto_data) <- c("cost", "production_time", "response_time", "reliability", "detection_rate", "video_quality")

# Visualize Pareto front: Response Time vs Reliability
ggplot(pareto_data, aes(x = response_time, y = reliability)) +
  geom_point() +
  labs(title = "Pareto Front: Response Time vs Reliability", x = "Response Time", y = "Reliability")

# Print each solution on the Pareto front and its corresponding architecture
pareto_solutions <- result$par
for (i in 1:nrow(pareto_solutions)) {
  bitstring <- paste(as.integer(pareto_solutions[i, ]), collapse = "")
  repaired_bitstring <- repair_bits(bitstring)
  architecture <- bitstring_to_architecture(repaired_bitstring)
  cat("Solution", i, ":\n")
  print(architecture)
  cat("Bitstring:", bitstring, "\nRepaired Bitstring:", repaired_bitstring, "\n")
  cat("\n")
}


