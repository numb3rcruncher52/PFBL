##############################################################
# DMB_analyze_results.R
#
# Analyze actual results compared to real results
##############################################################

## Calculate the rate of SNG, DBL, TRI, and HR based on real life
stats_rates <- stats %>%
  mutate(PA = calcPA(AB, UBB, HBP, SF)
         , SNG_rate = SNG / PA
         , DBL_rate = DBL / PA
         , TRI_rate = TRI / PA
         , HR_rate = HR / PA) %>%
  select(ID, Name, role, split, season, SNG_rate:HR_rate)

results_rates <- results %>%
  left_join(stats_rates) %>%
  mutate(real_PA = calcPA(AB, UBB, HBP, SF)
         , SNG_proj = real_PA * SNG_rate
         , DBL_proj = real_PA * DBL_rate
         , TRI_proj = real_PA * TRI_rate
         , HR_proj = real_PA * HR_rate) %>%
  left_join(select(batter_ratings, ID, Name, season, Bats))

write_csv(results_rates, "results_rates.csv")
