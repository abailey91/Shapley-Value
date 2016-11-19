
#computes the number of non-exposed, non-converting users to add
FinitePopulationCorrection <- function(unique_reach,population_size){
  round(population_size*sqrt((population_size-unique_reach)/(population_size-1)))
}