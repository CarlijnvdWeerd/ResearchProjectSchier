### script to analyse biometric data

ring_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQAgtmW1VexFUfTdRqa3r15Qke29Kd9niyy0w0wtojaSNmlyR9kdLcC2XqV5XzVZnL9m0tTdBybK0RC/pub?gid=1616529556&single=true&output=csv")

observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQl5pI4xRYpDKPVxhR_9XzzAtZetAqFbsSrpdtAzhqFHmvlBKZD0m9s-5jymajZGA/pub?gid=746495893&single=true&output=csv")

ring_data <- ring_data |>
  dplyr::mutate(Strategy = case_when(
    Flagcode %in% c("Of-AUC/R", "Of-CLC/R", "Of-CTC/R", "Of-ECC/R", "Of-JEL/R", "Of-JLT/R", "Of-JNL/R", "Of-JPU/R", "Of-JTN/R", "Of-JYL/R","Of-KJU/R", "Of-KKN/R", "Of-KNP/R", "Of-KPM/R", "Of-KTJ/R", "Of-KXM/R", "Of-LAV/R", "Of-LCP/R", "Of-LCT/R", "Of-LEH/R","Of-LHE/R", "Of-LLM/R", "Of-LLP/R", "Of-LMN/R", "Of-LNJ/R", "Of-LPC/R", "Of-LPE/R", "Of-LPL/R", "Of-PLT/R", "Of-LPT/R", "Of-LTA/R", "Of-LTV/R", "Of-LUC/R", "Of-LYK/R", "Of-MAU/R", "Of-MCH/R", "Of-MCU/R", "Of-MEY/R", "Of-MHN/R", "Of-MKE/R", "Of-MKY/R", "Of-MNN/R", "Of-MPC/R", "Of-MPV/R", "Of-MTJ/R", "Of-MYV/R", "Of-NJM/R", "Of-NJU/R", "Of-NLJ/R", "Of-NNC/R", "Of-NPE/R", "Of-NTV/R", "Of-NUM/R", "Of-NUN/R", "Of-NVK/R", "Of-PAE/R", "Of-PAJ/R", "Of-PAC/R", "Of-PAM/R", "Of-PHN/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R", "Of-PPT/R", "Of-PTA/R", "Of-PVA/R", "Of-PXX/R", "Of-PYC/R")  ~ "overwinterer",
    Flagcode %in% c("Of-CCU/R", "Of-HPL/R", "Of-HPV/R", "Of-JAE/R", "Of-JHL/R", "Of-JHM/R", "Of-JHY/R", "Of-JLU/R", "Of-JNN/R", "Of-JTC/R","Of-JXM/R", "Of-KCY/R", "Of-KET/R", "Of-KKH/R", "Of-KMC/R", "Of-KMY/R", "Of-KNX/R", "Of-KPH/R", "Of-KTP/R", "Of-KYM/R", "Of-MCV/R", "Of-MJA/R", "Of-NKA/R", "Of-NNP/R", "Of-NUK/R", "Of-PCU/R", "Of-PLA/R", "Of-PNV/R", "Of-PVK/R") ~ "early_northward_migration",
    Flagcode %in% c("Of-AVM/R", "Of-HNW/R", "Of-JCH/R", "Of-JHN/R", "Of-JJJ/R", "Of-JJV/R", "Of-JKM/R", "Of-JLH/R", "Of-JMN/R", "Of-JMU/R", "Of-JNH/R", "Of-JNM/R", "Of-JPX/R", "Of-JVH/R", "Of-JVL/R", "Of-JXN/R", "Of-JXY/R", "Of-KAH/R", "Of-KAX/R", "Of-KCT/R", "Of-KEN/R", "Of-KHE/R", "Of-KHK/R", "Of-KHU/R", "Of-KKM/R", "Of-KKT/R", "Of-KLP/R", "Of-KMT/R", "Of-KMV/R", "Of-KPN/R", "Of-KPU/R", "Of-KTE/R", "Of-KTK/R", "Of-KTN/R", "Of-KTM/R", "Of-KTT/R", "Of-KTV/R", "Of-KUH/R", "Of-KUL/R", "Of-KUX/R", "Of-KVA/R", "Of-KVC/R", "Of-KVH/R", "Of-KVV/R", "Of-KXC/R", "Of-KXV/R", "Of-KYC/R", "Of-LAJ/R", "Of-LCV/R", "Of-LEA/R", "Of-MAC/R", "Of-MCP/R", "Of-MJM/R", "Of-MLC/R", "Of-MXL/R", "Of-NEL/R", "Of-NKE/R", "Of-NLL/R", "Of-NNN/R", "Of-NPA/R", "Of-NPP/R", "Of-NPY/R", "Of-NUU/R", "Of-NXP/R", "Of-NYA/R", "Of-PAA/R", "Of-PHT/R", "Of-PJC/R", "Of-PJU/R", "Of-PLK/R", 
"Of-PXA/R") ~ "late_northward_migration",
    TRUE ~ "not_seen"))

p1a <- ggplot(ring_data,
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Surface pecking by Strategy",
    x = "Week",
    y = "Duration Rate") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none")
p6a      
      
      
     



#filmed_ring_data <- ring_data |>
# filter(Flagcode %in%
          c("Of-CCU/R", "Of-HPL/R", "Of-HPT/R", "Of-HNW/R", "Of-HPV/R", 
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

  
  
