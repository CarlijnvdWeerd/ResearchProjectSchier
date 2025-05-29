## Script with ringed individuals that were filmed

ringed_info <- observation_data |>
  filter(bird_code %in% c(
    "Of-CCU/R", "Of-HPL/R", "Of-HPT/R", "Of-HNW/R", "Of-HPV/R", 
    "Of-JAE/R", "Of-JCH/R", "Of-JHM/R", "Of-JHY/R", "Of-JJV/R", 
    "Of-JLH/R", "Of-JLO/R", "Of-JLU/R", "Of-JNH/R", "Of-JTL/R", 
    "Of-JTN/R", "Of-JVY/R", "Of-JXM/R", "Of-JXN/R", "Of-JXY/R", 
    "Of-JYL/R",  "Of-KAH/R", "Of-KCY/R", "Of-KET/R", "Of-KHK/R", 
    "Of-KJU/R", "Of-KKH/R", "Of-KKM/R", "Of-KKT/R", "Of-KLP/R", 
    "Of-KMC/R", "Of-KMY/R", "Of-KNP/R", "Of-KNX/R", "Of-KPM/R", 
    "Of-KTE/R",  "Of-KTM/R",  "Of-KTP/R", "Of-KUH/R", "Of-KUL/R", 
    "Of-KUX/R", "Of-KVV/R", "Of-KXM/R", "Of-KXV/R",   "Of-LCT/R", 
    "Of-LEA/R", "Of-LMN/R", "Of-LPT/R", "Of-LYK/R", "Of-MAU/R", 
    "Of-MCH/R", "Of-MCV/R", "Of-MJA/R", "Of-MLC/R", 
    "Of-MTJ/R", "Of-MYV/R", "Of-NEL/R", "Of-NKE/R", "Of-NLJ/R", 
    "Of-NLY/R", "Of-NPA/R", "Of-NPP/R", "Of-NPY/R", "Of-NTV/R", 
    "Of-NUK/R",  "Of-NVH/R", "Of-NYA/R", "Of-PAE/R", "Of-PAJ/R", 
    "Of-PHT/R", "Of-PJU/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R", 
    "Of-PLA/R",   "Of-PVK/R" 
  ))


ringed_info <- ringed_info |>
  dplyr::select(bird_code, observation_date, year, category)
