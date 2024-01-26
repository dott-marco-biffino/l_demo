# ch_08: Formazione e scioglimento delle coppie ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

pop_res_2023<- read_csv("data/ISTAT_POP_RES_2023.csv")

names(pop_res_2023)

## Donne ####

pop_res_2023_f <- pop_res_2023 |>
  mutate(Eta = as.numeric(`Età`),
         Nubili = Nubili,
         Coniugate = Coniugate + `Unite civilmente`,
         Altro = Divorziate + Vedove + 
           `Femmine già in unione civile (per scioglimento unione)`+
           `Femmine già in unione civile (per decesso del partner)`,
         Donne = Nubili + Coniugate + Altro,
         P_Nubili = 100 * Nubili / Donne,
         P_Coniugate = 100 * Coniugate / Donne,
         P_Altro = 100 * Altro / Donne,
         .keep = "none") |>
  slice_head(n = 100) |>
  select(Eta, starts_with("P_")) |>
  pivot_longer(cols = starts_with("P_"), 
               names_to = "StCiv", 
               values_to = "Percentuale") |>
  mutate(StCiv = factor(stringr::str_remove(StCiv, "P_"),
                           levels = c("Nubili", "Coniugate", "Altro")))

pop_res_f_eta_stciv<- ggplot(pop_res_2023_f,
        aes(x = Eta, 
            y = Percentuale,
            col = StCiv)) +
  geom_line() + 
  scale_color_manual(values = c("darkgreen", "red3", "slateblue4")) +
  labs(#title = "Donne per età e stato civile - Italia 01/01/2023", 
       x = "Età", y = "Percentuale", col = "Stato Civile") +
  geom_vline(xintercept = c(15, 50), 
             lty = 2) +
  xlim(0, 75) 

ggsave(filename = "figures/ch08_pop_res_f_eta_stciv.png",
       plot = pop_res_f_eta_stciv)

## Uomini ####

pop_res_2023_m <- pop_res_2023 |>
  mutate(Eta = as.numeric(`Età`),
         Celibi = Celibi,
         Coniugati = Coniugati + `Uniti civilmente`,
         Altro = Divorziati + Vedovi + 
           `Maschi già in unione civile (per scioglimento unione)`+
           `Maschi già in unione civile (per decesso del partner)`,
         Uomini = Celibi + Coniugati + Altro,
         P_Celibi = 100 * Celibi / Uomini,
         P_Coniugati = 100 * Coniugati / Uomini,
         P_Altro = 100 * Altro / Uomini,
         .keep = "none") |>
  slice_head(n = 100) |>
  select(Eta, starts_with("P_")) |>
  pivot_longer(cols = starts_with("P_"), 
               names_to = "StCiv", 
               values_to = "Percentuale") |>
  mutate(StCiv = factor(stringr::str_remove(StCiv, "P_"),
                        levels = c("Celibi", "Coniugati", "Altro")))

pop_res_m_eta_stciv<- ggplot(pop_res_2023_m,
       aes(x = Eta, 
           y = Percentuale,
           col = StCiv)) +
  geom_line() + 
  scale_color_manual(values = c("darkgreen", "red3", "slateblue4")) +
  labs(#title = "Donne per età e stato civile - Italia 01/01/2023", 
    x = "Età", y = "Percentuale", col = "Stato Civile") +
  geom_vline(xintercept = c(15, 50), 
             lty = 2) +
  xlim(0, 75) 

ggsave(filename = "figures/ch08_pop_res_m_eta_stciv.png",
       plot = pop_res_m_eta_stciv)
