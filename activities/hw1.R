# Write a function that computes energy produced from a photovoltaic system if you know the average annual solar radiation

# r is panel yield (0-1) manufacturing efficiency is 0.2
r <- 0.2

# performance ratio is (accounting for site factors that impact efficiency usually around .75)
pr <- 0.75

# average solar panel area in meters squared, 1.6 meters squared according to
A <- 1.6

# we know annual radiation, h, in kWh
compute_energy <- function(h){
  energy <- A * r * h * pr # function to find E for energy
  paste(energy, "kWh of energy is produced from", h, "kWh of annual radiation.") # energy in kwh
}

compute_energy(3) # say we have 3 kWh of annual radiation, and want to see the output for how much energy is produced
