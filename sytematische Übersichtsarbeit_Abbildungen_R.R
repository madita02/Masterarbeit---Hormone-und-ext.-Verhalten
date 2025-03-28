# # Bibliotheken laden
library(ggplot2)
library(dplyr)
library(readxl)
library(extrafont)


### Abbildung 7: Korrelationskoeffizieten von Testosteron und externalisierendem Verhalten getrennt nach Subskalen und Geschlecht

###Teststeron (EMF) 

# Excel-Datei laden
df_t <- read_excel("C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Datenanalyse/Extraction_MA_CBCL_R.xlsx", sheet = 5)

# Pakete laden
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)
library(devEMF)
library(scales)

df_t <- df_t %>%
  mutate(across(c("n_total", "mean_age_total", "mean_age_male", "mean_age_female",
                  "sd_age_total", "sd_age_male","sd_age_female","n_male","n_female",
                  "r_hormone_phenotype_T1", "r_hormone_phenotype_male", "r_hormone_phenotype_female",
                  "ROB_total_out_of_6"), ~ as.numeric(.)))

# APA-konformes Theme
apa_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    legend.position = "right",
    legend.justification = "left",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "inches")
  )


# Plot
create_plot <- function(data, y_column, title_label) {
  
  col_sym <- rlang::sym(y_column)
  
  ggplot(
    data,
    aes(
      x = interaction(surname_first_autor_year, Scale, Subscale, sep = " - "),
      y = !!col_sym,
      fill = Subscale
    )
  ) +
    geom_bar(
      stat = "identity", 
      width = 0.6,  
      color = "black", 
      position = position_dodge(width = 0.6)
    ) +
    
    geom_text(
      aes(
        label = Scale,
        y = ifelse(!!col_sym > 0,
                   !!col_sym + 0.05,
                   ifelse(!!col_sym < 0,
                          !!col_sym - 0.1,
                          0))
      ),
      angle = 90,
      vjust = ifelse(!!col_sym > 0, 0, ifelse(!!col_sym < 0, 1.2, 0.5)),
      hjust = 0.3,  
      position = position_dodge(width = 0.6),
      size = 4
    ) +
    scale_fill_manual(
      values = c("ext" = "#377eb8", "agg" = "#e41a1c", "rubr" = "#4daf4a"),
      labels = c(
        "ext"  = "Externalisierendes Verhalten", 
        "agg"  = "Aggressives Verhalten", 
        "rubr" = "Regelbrechendes Verhalten"
      )
    ) +
    scale_y_continuous(
      limits = c(-0.35, 0.65),
      breaks = seq(-0.35, 0.65, by = 0.1),
      labels = scales::label_number(accuracy = 0.01)
    )+
    labs(
      title = title_label,
      x = "Studien Testosteron",
      y = "Korrelationskoeffizient",
      fill = "Legende:"
    ) +
    scale_x_discrete(
      labels = function(x) sub(" - .*", "", x)
    ) +
    apa_theme
}

plot_data_mixed <- df_t %>%
  filter(Subscale %in% c("ext", "agg", "rubr")) %>%
  filter(!is.na(r_hormone_phenotype_T1)) %>%
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_T1) %>%
  mutate(
    surname_first_autor_year = factor(surname_first_autor_year),
    Subscale = factor(Subscale, levels = c("ext", "agg", "rubr")),
    Scale = factor(Scale)
  )

plot_data_male <- df_t %>%
  filter(Subscale %in% c("ext", "agg", "rubr")) %>%
  filter(!is.na(r_hormone_phenotype_male)) %>%
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_male) %>%
  mutate(
    surname_first_autor_year = factor(surname_first_autor_year),
    Subscale = factor(Subscale, levels = c("ext", "agg", "rubr")),
    Scale = factor(Scale)
  )

plot_data_female <- df_t %>%
  filter(Subscale %in% c("ext", "agg", "rubr")) %>%
  filter(!is.na(r_hormone_phenotype_female)) %>%
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_female) %>%
  mutate(
    surname_first_autor_year = factor(surname_first_autor_year),
    Subscale = factor(Subscale, levels = c("ext", "agg", "rubr")),
    Scale = factor(Scale)
  )
# Plots erstellen

plot_mixed <- create_plot(plot_data_mixed, "r_hormone_phenotype_T1", "Mixed") +
  theme(legend.position = "none")

plot_male <- create_plot(plot_data_male, "r_hormone_phenotype_male", "Männlich") +
  theme(legend.position = "none")

plot_female <- create_plot(plot_data_female, "r_hormone_phenotype_female", "Weiblich") +
  theme(legend.position = "none")

# Gemeinsame Legende
legend_common <- get_legend(
  create_plot(plot_data_male, "r_hormone_phenotype_male", "Männlich") +
    theme(
      legend.position = "right",
      legend.justification = "left"
    )
)
legend_plot <- ggdraw(legend_common)
# 2×2 Layout
layout_design <- "
AB
CD
"

combined_plot_test <- plot_mixed + plot_male + plot_female + legend_plot +
  plot_layout(design = layout_design, widths = c(1, 0.8), heights = c(1, 1))

# EMF Export
output_path <- "C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Masterarbeit/combined_plot_test.emf"

emf(file = output_path, width = 12, height = 16, family = "Arial")
print(combined_plot_test)
dev.off()


###########################################################################

# Abbildung 8: Korrelationskoeffizieten von DHEA und externalisierendem Verhalten getrennt nach Subskalen und Geschlecht

#DHEA (EMF)
# Notwendige Pakete laden
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(devEMF) 

# Excel-Datei laden
df_d <- read_excel("C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Datenanalyse/Extraction_MA_CBCL_R.xlsx", sheet = 3)

df_d <- df_d %>%
  mutate(across(c("n_total", "mean_age_total", "mean_age_male", "mean_age_female",
                  "sd_age_total", "sd_age_male","sd_age_female","n_male","n_female",
                  "r_hormone_phenotype_T1", "r_hormone_phenotype_male","r_hormone_phenotype_female",
                  "ROB_total_out_of_6"), as.numeric))

create_plot <- function(data, y_column, title, show_legend_title = FALSE) {
  ggplot(data, aes(x = surname_first_autor_year, y = !!sym(y_column), fill = Subscale)) +
    geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9), color = "black") +
    geom_text(
      aes(
        label = Scale,
        y = ifelse(!!sym(y_column) > 0, !!sym(y_column) + 0.06, 
                   ifelse(!!sym(y_column) < 0, !!sym(y_column) - 0.06, !!sym(y_column))),
        group = Subscale
      ),
      position = position_dodge(width = 0.9),
      angle = 90,
      size = 3
    ) +
    scale_fill_manual(
      values = c("ext" = "#377eb8", "agg" = "#e41a1c", "rubr" = "#4daf4a"),
      labels = c("ext" = "Externalisierendes Verhalten", 
                 "agg" = "Aggressives Verhalten", 
                 "rubr" = "Regelbrechendes Verhalten")
    ) +
    scale_y_continuous(limits = c(-0.4, 0.45), expand = expansion(mult = c(0, 0.1))) + 
    labs(
      title = title,
      x = "Studien DHEA",
      y = "Korrelationskoeffizient",
      fill = if (show_legend_title) "Legende: " else NULL 
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.key.height = unit(0.5, "cm"),  
      legend.spacing.y = unit(0.5, "cm"),   
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 1))  
}

plot_data_mixed <- df_d %>%
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_T1) %>%
  filter(Subscale %in% c("ext", "agg", "rubr")) %>%
  filter(!is.na(r_hormone_phenotype_T1)) %>%
  mutate(surname_first_autor_year = factor(surname_first_autor_year),
         Subscale = factor(Subscale, levels = c("ext", "agg", "rubr")),
         Scale = factor(Scale))

plot_data_male <- df_d %>%
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_male) %>%
  filter(Subscale %in% c("ext", "agg", "rubr")) %>%
  filter(!is.na(r_hormone_phenotype_male)) %>%
  mutate(surname_first_autor_year = factor(surname_first_autor_year),
         Subscale = factor(Subscale, levels = c("ext", "agg", "rubr")),
         Scale = factor(Scale))

plot_data_female <- df_d %>%
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_female) %>%
  filter(Subscale %in% c("ext", "agg", "rubr")) %>%
  filter(!is.na(r_hormone_phenotype_female)) %>%
  mutate(surname_first_autor_year = factor(surname_first_autor_year),
         Subscale = factor(Subscale, levels = c("ext", "agg", "rubr")),
         Scale = factor(Scale))

# Plots erstellen
plot_mixed <- create_plot(plot_data_mixed, "r_hormone_phenotype_T1", "Mixed", show_legend_title = TRUE)
plot_male <- create_plot(plot_data_male, "r_hormone_phenotype_male", "Männlich", show_legend_title = FALSE)
plot_female <- create_plot(plot_data_female, "r_hormone_phenotype_female", "Weiblich", show_legend_title = FALSE)

combined_plot <- (plot_mixed | plot_male | plot_female) + 
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom")

# EMF-Datei speichern 
emf(file = "C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Masterarbeit/combined_plot_dhea.emf",
    width = 6.5, height = 6, family = "Arial")
print(combined_plot)
dev.off()

#################################################################################

# Abbildung 9: Korrelationskoeffizieten von Estradiol und externalisierendem Verhalten getrennt nach Subskalen und Geschlecht

#Estradiol (EMF)

# Notwendige Pakete laden
library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)
library(devEMF)

# Excel-Datei laden
df <- read_excel("C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Datenanalyse/Extraction_MA_CBCL_R.xlsx", sheet = 4)

# Umwandlung relevanter Spalten in numerische Werte
df <- df %>%
  mutate(across(c("r_hormone_phenotype_T1", "r_hormone_phenotype_male", "r_hormone_phenotype_female"), 
                as.numeric))

df$Subscale <- factor(df$Subscale, levels = c("ext", "agg", "rubr"))  

y_limits <- range(c(df$r_hormone_phenotype_T1, 
                    df$r_hormone_phenotype_male, 
                    df$r_hormone_phenotype_female), na.rm = TRUE) * 1.3

create_plot <- function(data, y_column, title, legend_position = "none") {
  ggplot(data, aes(
    x = interaction(surname_first_autor_year, Scale, Subscale, sep = " - "), 
    y = !!sym(y_column), fill = Subscale)) +
    
    geom_bar(stat = "identity", width = 0.6, color = "black", position = position_dodge(width = 0.9)) + 
    
    geom_text(aes(
      label = Scale, 
      y = ifelse(!!sym(y_column) > 0, !!sym(y_column) + 0.04, 
                 ifelse(!!sym(y_column) < 0, !!sym(y_column) - 0.04, !!sym(y_column)))), 
      angle = 90, size = 5, family = "Arial", vjust = 0.4, hjust = 0.5) +  
    
    scale_fill_manual(
      values = c("ext" = "#377eb8", "agg" = "#e41a1c", "rubr" = "#4daf4a"), 
      labels = c("Externalisierendes Verhalten", "Aggressives Verhalten", "Regelbrechendes Verhalten")
    ) +
    
    labs(
      title = title,
      x = "Studien Estradiol",
      y = "Korrelationskoeffizient",
      fill = "Legende:"
    ) +
    
    # Entfernt " - ext", " - agg", " - rubr" UND "CBCL", "YSR", "TRF"
    scale_x_discrete(
      labels = function(x) gsub(" - (ext|agg|rubr|CBCL|YSR|TRF)", "", x)
    ) +
    
    scale_y_continuous(
      limits = c(-0.15, 0.2),  
      labels = scales::label_number(accuracy = 0.01)  
    )+ 
    
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", size = 14),
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),  
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
      axis.text.y = element_text(size = 12), 
      axis.title.x = element_text(size = 14), 
      axis.title.y = element_text(size = 14),
      legend.position = legend_position
    )
}

plot_data_mixed <- df %>%
  filter(!is.na(r_hormone_phenotype_T1)) %>%
  arrange(Subscale) %>%  
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_T1)

plot_data_male <- df %>%
  filter(!is.na(r_hormone_phenotype_male)) %>%
  arrange(Subscale) %>%  
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_male)

plot_data_female <- df %>%
  filter(!is.na(r_hormone_phenotype_female)) %>%
  arrange(Subscale) %>%  
  select(surname_first_autor_year, Subscale, Scale, r_hormone_phenotype_female)

plot_mixed <- create_plot(plot_data_mixed, "r_hormone_phenotype_T1", "Mixed", legend_position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +  
  theme(
    legend.position = "bottom", 
    legend.justification = "center", 
    legend.box = "vertical"  
  )

plot_male <- create_plot(plot_data_male, "r_hormone_phenotype_male", "Männlich", legend_position = "none")
plot_female <- create_plot(plot_data_female, "r_hormone_phenotype_female", "Weiblich", legend_position = "none")

combined_plot <- (plot_mixed + plot_male + plot_female) +
  plot_layout(ncol = 3, widths = c(1.2, 1, 1.7), guides = "collect") & 
  theme(
    legend.position = "bottom", 
    legend.justification = "center", 
    legend.box = "vertical"
  )

final_plot <- combined_plot + plot_layout(heights = c(4, 0.7))  

# Speichern als EMF
emf(file = "C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Masterarbeit/combined_plot_estr.emf",
    width = 7, height = 8, family = "Arial")
print(final_plot)
dev.off()

##############################################################################

# Abbildung 3: Stichprobengröße pro Studie und Risiko der Verzerrung (ROB)

#ROB (EMF)

# Erforderliche Pakete laden
library(ggplot2)
library(devEMF)
library(dplyr)

# Daten kombinieren
combined_df <- bind_rows(
  df_t %>% mutate(Source = "Testosteron"),
  df_d %>% mutate(Source = "DHEA"),
  df_e %>% mutate(Source = "Estradiol")
) %>%
  group_by(surname_first_autor_year) %>%
  summarise(
    n_total = max(n_total, na.rm = TRUE),
    ROB_total_out_of_6 = max(ROB_total_out_of_6, na.rm = TRUE),
    Source = paste(unique(Source), collapse = ", ")
  ) %>%
  ungroup()

# Tackett_2015 aus dem Datensatz entfernen
combined_df <- combined_df %>% filter(surname_first_autor_year != "Tackett_2015")


combined_df <- combined_df %>%
  mutate(ROB_total_out_of_6 = factor(ROB_total_out_of_6, levels = 0:6))

# Farben für ROB-Werte definieren
rob_colors <- c(
  "0" = "#D7191C",  
  "1" = "red",  
  "2" = "darkorange", 
  "3" = "#F7F700", 
  "4" = "#4DAC26",  
  "5" = "#2C7BB6",  
  "6" = "#008837"   
)

# Grafik erstellen 
plot_ROB <- ggplot(data = combined_df, aes(x = reorder(surname_first_autor_year, n_total),
                                           y = n_total,
                                           color = ROB_total_out_of_6)) +
  geom_point(shape = 16, size = 8, alpha = 0.8) +  
  scale_color_manual(
    values = rob_colors,
    name = "Risk of Bias (ROB)",  
    breaks = levels(combined_df$ROB_total_out_of_6),
    labels = c("0 = Extrem hoch", 
               "1 = Sehr hoch", 
               "2 = Hoch", 
               "3 = Mittel", 
               "4 = Niedrig", 
               "5 = Sehr niedrig", 
               "6 = Keine Verzerrung"),
    drop = TRUE 
  ) +
  labs(
    x = "Studien", 
    y = "Stichprobengröße"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    axis.title.y = element_text(size = 12, margin = margin(r = 8)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),  
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),  
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 5, alpha = 1),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  scale_y_continuous(limits = c(min(combined_df$n_total) * 0.95, max(combined_df$n_total) * 1.05))  

# EMF-Datei speichern
emf(file = "C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Masterarbeit/plot_ROB.emf",
    width = 10, height = 5, family = "Arial")  

print(plot_ROB)
dev.off()

#######################################################################################

#Ahang D: Risiko der Verzerrung der ABCD-Studie vs. Studien der systematischen Übersichtsarbeit 

#ROB mit ABCD (EMF)

# Erforderliche Pakete laden
library(ggplot2)
library(devEMF)
library(dplyr)

# Daten kombinieren
combined_df <- bind_rows(
  df_t %>% mutate(Source = "Testosteron"),
  df_d %>% mutate(Source = "DHEA"),
  df_e %>% mutate(Source = "Estradiol")
) %>%
  group_by(surname_first_autor_year) %>%
  summarise(
    n_total = max(n_total, na.rm = TRUE),
    ROB_total_out_of_6 = max(ROB_total_out_of_6, na.rm = TRUE),
    Source = paste(unique(Source), collapse = ", ")
  ) %>%
  ungroup()

# Tackett_2015 aus dem Datensatz entfernen
combined_df <- combined_df %>% filter(surname_first_autor_year != "Tackett_2015")

combined_df <- combined_df %>%
  mutate(ROB_total_out_of_6 = factor(ROB_total_out_of_6, levels = 1:4))

# Manuell den Punkt für "ABCD-Studie" hinzufügen
manual_point <- tibble(
  surname_first_autor_year = "ABCD-Studie",
  n_total = 422,
  ROB_total_out_of_6 = factor(2, levels = 1:4),
  Source = "Manuell hinzugefügt"
)

combined_df <- bind_rows(combined_df, manual_point)

rob_colors <- c(
  "1" = "red",         
  "2" = "darkorange",  
  "3" = "#F7F700",     
  "4" = "#4DAC26"     
)

x_labels <- levels(reorder(combined_df$surname_first_autor_year, combined_df$n_total))
x_labels <- ifelse(x_labels == "ABCD-Studie", expression(bold("ABCD-Studie")), x_labels)

plot_ROB_ABCD <- ggplot(data = combined_df, aes(x = reorder(surname_first_autor_year, n_total),
                                                y = n_total,
                                                color = ROB_total_out_of_6)) +
  geom_point(shape = 16, size = 10, alpha = 0.8) + 
  scale_color_manual(
    values = rob_colors,
    name = "ROB-Wert", 
    breaks = levels(combined_df$ROB_total_out_of_6),
    labels = c("1 = Sehr hoch", 
               "2 = Hoch", 
               "3 = Mittel", 
               "4 = Niedrig"),
    drop = TRUE 
  ) +
  scale_x_discrete(labels = x_labels) + 
  labs(
    x = "Studien", 
    y = "Stichprobengröße"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),  
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 5, alpha = 1),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  scale_y_continuous(limits = c(min(combined_df$n_total) * 0.95, max(combined_df$n_total) * 1.05))  

# EMF-Datei speichern 
emf(file = "C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Masterarbeit/plot_ROB_ABCD.emf",
    width = 10, height = 5, family = "Arial")  

print(plot_ROB_ABCD)
dev.off()


##################################################################################


#Abbildung 4: Alterspanne der Studienteilnehmenden pro Studie gentrennt nach Geschlecht

## Altersspanne pro Studie (EMF)

# Notwendige Bibliotheken laden
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(devEMF) 

df_alter <- read_excel("C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Datenanalyse/Extraction_MA_CBCL_R.xlsx", sheet = 2)

df_a <- df_alter[, c("surname_first_autor_year", "mean_age_total", "mean_age_male", "mean_age_female", "sd_age_total", "sd_age_male", "sd_age_female")]

df_ua <- df_a %>% distinct(surname_first_autor_year, .keep_all = TRUE)

df_al <- df_ua %>%
  pivot_longer(
    cols = starts_with("mean_age") | starts_with("sd_age"),
    names_to = c("Measure", "Group"),
    names_pattern = "(.+)_([^_]+)$"
  ) %>%
  pivot_wider(names_from = Measure, values_from = value) %>%
  mutate(
    mean_age = as.numeric(mean_age),
    sd_age = as.numeric(sd_age)
  ) %>%
  filter(!is.na(mean_age))

# Plot erstellen
plot_Alter <- ggplot(df_al, aes(y = surname_first_autor_year, x = mean_age, color = Group, shape = Group)) +
  geom_segment(aes(
    x = mean_age - sd_age, xend = mean_age + sd_age,
    y = as.numeric(factor(surname_first_autor_year)) + c(-0.2, 0.2, 0)[as.numeric(factor(Group))],
    yend = as.numeric(factor(surname_first_autor_year)) + c(-0.2, 0.2, 0)[as.numeric(factor(Group))],
    linetype = Group
  ), linewidth = 1, na.rm = TRUE) +
  geom_point(aes(
    x = mean_age,
    y = as.numeric(factor(surname_first_autor_year)) + c(-0.2, 0.2, 0)[as.numeric(factor(Group))]
  ), size = 3, na.rm = TRUE) +
  scale_color_manual(
    name = "Geschlecht",
    values = c("total" = "black", "male" = "blue", "female" = "red"),
    labels = c("total" = "total", "male" = "männlich", "female" = "weiblich")
  ) +
  scale_linetype_manual(
    name = "Geschlecht",
    values = c("total" = "solid", "male" = "dotted", "female" = "dashed"),
    labels = c("total" = "total", "male" = "männlich", "female" = "weiblich")
  ) +
  scale_shape_manual(
    name = "Geschlecht",
    values = c("total" = 16, "male" = 17, "female" = 15),
    labels = c("total" = "total", "male" = "männlich", "female" = "weiblich")
  ) +
  scale_x_continuous(breaks = seq(7, 18, 1)) +
  scale_y_continuous(breaks = as.numeric(factor(df_al$surname_first_autor_year)), labels = df_al$surname_first_autor_year) +
  labs(
    title = "",
    x = "Alter (Standardabweichung)",
    y = "Studie"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

# EMF-Datei speichern
emf(file = "C:/Users/mmert/OneDrive/Desktop/Masterarbeit/Masterarbeit/plot_Alter.emf", 
    width = 10, height = 6, family = "Arial")

print(plot_Alter)
dev.off() 
