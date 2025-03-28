#### ABCD-Sekundäranalyse

###Datensätze hochladen 
install.packages("showtext")
library(systemfonts)

#Daten hochladen
df <- readRDS("//fp-file-host-1.psycho.unibas.ch/cds_abcd/output/Madita/madita.rds")

#Daten ABCD-Demo
df_demo <- read.csv("//fp-file-host-1.psycho.unibas.ch/cds_abcd/output/Madita/abcd_p_demo.csv")

#Daten ABCD-y 
df_y <- read.csv("//fp-file-host-1.psycho.unibas.ch/cds_abcd/output/Madita/abcd_y_lt.csv")
########

### Tabellen erstellen 
# Id, Upps(Jahr3), CBCL(Jahr4), Hormone(Jahr 2)
library(dplyr)

table(df$eventname, useNA = "ifany")


# Hormone (T3), UPPS(T5), CBCL (T5)
# Filter für jede gewünschte Jahr-Variable
t_2 <- df %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(src_subject_id, residuals_test)

d_2 <- df %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(src_subject_id, residuals_dhea)
e_2 <- df %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(src_subject_id, residuals_estr)

CBCL_4 <- df %>%
  filter(eventname == "4_year_follow_up_y_arm_1") %>%
  select(src_subject_id, cbcl_scr_syn_rulebreak_r, cbcl_scr_syn_rulebreak_m, cbcl_scr_syn_rulebreak_nm,
         cbcl_scr_syn_aggressive_r, cbcl_scr_syn_aggressive_m, cbcl_scr_syn_aggressive_nm, 
         cbcl_scr_syn_external_r, cbcl_scr_syn_external_m, cbcl_scr_syn_external_nm)


UPPS_4 <- df %>%
  filter(eventname == "4_year_follow_up_y_arm_1") %>%
  select(src_subject_id, upps_y_ss_lack_of_perseverance, upps_y_ss_lack_of_planning, upps_y_ss_sensation_seeking,
         upps_y_ss_negative_urgency,  upps_y_ss_positive_urgency)


#### für jedes Hormon eigene Tabelle 
# Tabelle zu Testosteron 
df_t <- t_2%>%
  left_join(CBCL_4, by = "src_subject_id") %>%
  left_join(UPPS_4, by = "src_subject_id")

# DHEA
df_d <- d_2%>%
  left_join(CBCL_4, by = "src_subject_id") %>%
  left_join(UPPS_4, by = "src_subject_id")

#Estradiol
df_e <- e_2%>%
  left_join(CBCL_4, by = "src_subject_id") %>%
  left_join(UPPS_4, by = "src_subject_id")



###### Korrealtion zw. sex und Gender

# Neue Tabelle mit den gewünschten Spalten
df_sex <- df_demo[, c("demo_sex_v2", "demo_gender_id_v2")]

# Zeilen mit NA-Werten entfernen
df_sex <- na.omit(df_sex)

#numerische Werte 
df_sex$demo_sex_v2 <- as.numeric(as.factor(df_sex$demo_sex_v2))
df_sex$demo_gender_id_v2 <- as.numeric(as.factor(df_sex$demo_gender_id_v2))

#Korrelation von Sex und gender --> 0.93 
cor(df_sex$demo_sex_v2, df_sex$demo_gender_id_v2,use = "complete.obs")


###############
#einzelne Hormontabellen um Sex und Alter in Monaten erweitern 

# nur geschelcht 
df_geschlecht <- df_demo %>%
  filter(eventname == "baseline_year_1_arm_1") %>%  
  select(src_subject_id, demo_sex_v2)          

library(dplyr)
library(tidyr)

df_alter <- df_y %>%
  filter(eventname %in% c("4_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1")) %>%
  select(src_subject_id, eventname, interview_age) %>%
  pivot_wider(
    names_from = eventname, 
    values_from = interview_age, 
    names_prefix = "age_"
  )

#### Datensätze anhand der ID zusammensetzen 
## df-Testosteron 
df_t <- merge(df_t, df_alter, by = "src_subject_id")
df_t <- merge(df_t, df_geschlecht, by = "src_subject_id")

###df-DHEA
df_d <- merge(df_d, df_alter, by = "src_subject_id")
df_d <- merge(df_d, df_geschlecht, by = "src_subject_id")

###df-Estradiol
df_e <- merge(df_e, df_alter, by = "src_subject_id")
df_e <- merge(df_e, df_geschlecht, by = "src_subject_id")

### bei Estradiol nur Weiblich --> männlich entfernen m=1, w=2 
df_e_w <- df_e %>%
  filter(demo_sex_v2 != 1)

####NA-entfernen 
### df Testosteron 
# Entfernen, wenn alle Spalten  von CBCL NA sind --> 8 aussoritert wegen fehleden CBCL 
df_tf <- df_t %>%
  filter(rowSums(is.na(select(.,"cbcl_scr_syn_rulebreak_r", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_external_r"))) < 3)
###--> nur noch 409 --> anstatt 417

# alle ohne Testosteron-Wert entfernen 
df_tf <- df_tf %>%
  filter(!is.na(residuals_test))
### 6 entfernt weil kein Testtosteron 

#df DHEA 
# Entfernen, wenn alle Spalten  von CBCL NA sind --> 8 aussoritert wegen fehleden CBCL 
df_df <- df_d %>%
  filter(rowSums(is.na(select(.,"cbcl_scr_syn_rulebreak_r", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_external_r"))) < 3)
###--> nur noch 409 --> anstatt 417

# alle ohne DHEA-Wert entfernen 
df_df <- df_df %>%
  filter(!is.na(residuals_dhea))

#df Estradiol (nur weiblich)
# Entfernen, wenn alle Spalten  von CBCL NA sind --> 7 aussoritert wegen fehleden CBCL 
df_ef <- df_e_w %>%
  filter(rowSums(is.na(select(.,"cbcl_scr_syn_rulebreak_r", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_external_r"))) < 3)
###--> nur noch 189 --> anstatt 196

# alle ohne Estradiol-Wert entfernen 
df_ef <- df_ef %>%
  filter(!is.na(residuals_estr))
#### 6 entfernt weil kein estradiol 


#### zu den Datensätze noch die Missings von UPPS hinzufügen (fehlen bei df)
## alle die CBCL haben haben auch den completten UPPS

mh_y_upps<- read.csv("//fp-file-host-1.psycho.unibas.ch/cds_abcd/data_rv5_2024/mh_y_upps.csv")

df_UPPS <- mh_y_upps %>%
  filter(eventname == "4_year_follow_up_y_arm_1") %>%
  select(src_subject_id, upps_y_ss_negative_urgency_nm, upps_y_ss_lack_of_pers_nm,
         upps_y_ss_lack_of_planning_nm, upps_y_ss_sensation_seeking_nm, upps_y_ss_positive_urgency_nm)

## mit dfs verbinden 
df_tf<- merge(df_UPPS, df_tf, by = "src_subject_id")

df_df<- merge(df_UPPS, df_df, by = "src_subject_id")

df_ef<- merge(df_UPPS, df_ef, by = "src_subject_id")


## Alter von T3 und T5 korrelieren 
cor(df_tf$age_2_year_follow_up_y_arm_1,df_tf$age_4_year_follow_up_y_arm_1 )
cor(df_df$age_2_year_follow_up_y_arm_1,df_df$age_4_year_follow_up_y_arm_1 )
cor(df_ef$age_2_year_follow_up_y_arm_1,df_ef$age_4_year_follow_up_y_arm_1 )


# können Rohwerte verwendet werden bei CBCL und UPPS
lapply(df_tf, function(x) table(x, useNA = "ifany"))

lapply(df_df, function(x) table(x, useNA = "ifany"))

lapply(df_ef, function(x) table(x, useNA = "ifany"))
##--> Rohwerte können genommen werden --> keine NA-Werte in CBCL und UPPS 


####Korrelation zw. agg. und rubr. 
cor(df_tf$cbcl_scr_syn_aggressive_r, df_tf$cbcl_scr_syn_rulebreak_r)
cor(df_df$cbcl_scr_syn_aggressive_r, df_df$cbcl_scr_syn_rulebreak_r)
cor(df_ef$cbcl_scr_syn_aggressive_r, df_ef$cbcl_scr_syn_rulebreak_r)


## Rohwerte der UPPS-Subskalen in einen Rohwert aufsummieren
## Testosteron 
df_sum_tf<- aggregate(upps_y_ss_lack_of_perseverance + upps_y_ss_lack_of_planning +
                        upps_y_ss_sensation_seeking +upps_y_ss_negative_urgency +
                        upps_y_ss_positive_urgency~ src_subject_id, data = df_tf, sum)
names(df_sum_tf)[2] <- "UPPS"
df_tf <- merge(df_tf, df_sum_tf, by = "src_subject_id", all = TRUE)


## DHEA 
df_sum_df<- aggregate(upps_y_ss_lack_of_perseverance + upps_y_ss_lack_of_planning +
                        upps_y_ss_sensation_seeking +upps_y_ss_negative_urgency +
                        upps_y_ss_positive_urgency~ src_subject_id, data = df_df, sum)
names(df_sum_df)[2] <- "UPPS"
df_df <- merge(df_df, df_sum_df, by = "src_subject_id", all = TRUE)

## Estradiol 
df_sum_ef<- aggregate(upps_y_ss_lack_of_perseverance + upps_y_ss_lack_of_planning +
                        upps_y_ss_sensation_seeking +upps_y_ss_negative_urgency +
                        upps_y_ss_positive_urgency~ src_subject_id, data = df_ef, sum)
names(df_sum_ef)[2] <- "UPPS"
df_ef <- merge(df_ef, df_sum_ef, by = "src_subject_id", all = TRUE)

#### Spalten mit missing value + subskalen UPPS können entfernt werden 
#Testosteron 
df_testo<- df_tf %>%
  select(src_subject_id, residuals_test,cbcl_scr_syn_aggressive_r, cbcl_scr_syn_rulebreak_r, cbcl_scr_syn_external_r,
         UPPS, demo_sex_v2, age_2_year_follow_up_y_arm_1, age_4_year_follow_up_y_arm_1)

#DHEA
df_dhea<- df_df %>%
  select(src_subject_id, residuals_dhea,cbcl_scr_syn_aggressive_r, cbcl_scr_syn_rulebreak_r, cbcl_scr_syn_external_r,
         UPPS, demo_sex_v2, age_2_year_follow_up_y_arm_1, age_4_year_follow_up_y_arm_1)

#estradiol (ohne Variable Geschlecht)
df_estr<- df_ef %>%
  select(src_subject_id, residuals_estr,cbcl_scr_syn_aggressive_r, cbcl_scr_syn_rulebreak_r, cbcl_scr_syn_external_r,
         UPPS, age_2_year_follow_up_y_arm_1, age_4_year_follow_up_y_arm_1)



#### Datensätze von Testosteron und DHEA nach Geschlecht trennen 
df_testo_m <- subset(df_testo, demo_sex_v2 == 1)
df_testo_m <- df_testo_m %>% select(-c(demo_sex_v2))
df_testo_f <- subset(df_testo, demo_sex_v2 == 2)
df_testo_f <- df_testo_f %>% select(-c(demo_sex_v2))

## nochmal Korrelation agg und rubr 
cor(df_testo_m$cbcl_scr_syn_aggressive_r, df_testo_m$cbcl_scr_syn_rulebreak_r)
cor(df_testo_f$cbcl_scr_syn_aggressive_r, df_testo_f$cbcl_scr_syn_rulebreak_r)

# Datensatz DHEA 
df_dhea_m <- subset(df_dhea, demo_sex_v2 == 1)
df_dhea_m <- df_dhea_m %>% select(-c(demo_sex_v2))
df_dhea_f <- subset(df_dhea, demo_sex_v2 == 2)
df_dhea_f <- df_dhea_f %>% select(-c(demo_sex_v2))

cor(df_dhea_m$cbcl_scr_syn_aggressive_r, df_dhea_m$cbcl_scr_syn_rulebreak_r)
cor(df_dhea_f$cbcl_scr_syn_aggressive_r, df_dhea_f$cbcl_scr_syn_rulebreak_r)

# Estradiol
cor(df_ef$cbcl_scr_syn_aggressive_r, df_ef$cbcl_scr_syn_rulebreak_r)


#################################################################################
### Deskriptive Statistik der Stichprobe 
# Testoteorn 
library(dplyr)

# Deskriptive Statistik berechnen
sta_t <- df_testo %>%
  summarise(
    N_gesamt = n(),  # Gesamtanzahl der Stichprobe
    N_maennlich = sum(demo_sex_v2 == 1, na.rm = TRUE),  # Anzahl männlich (1)
    N_weiblich = sum(demo_sex_v2 == 2, na.rm = TRUE),  # Anzahl weiblich (2)
    Alter_Mean = mean(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Gesamtmittelwert Alter
    Alter_SD = sd(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Gesamtstandardabweichung Alter
    Alter_Min = min(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Minimales Alter
    Alter_Max = max(age_2_year_follow_up_y_arm_1, na.rm = TRUE)  # Maximales Alter
  )

# Deskriptive Statistik getrennt nach Geschlecht
sta_t_g <- df_testo %>%
  group_by(demo_sex_v2) %>%
  summarise(
    N = n(),
    Alter_Mean = mean(age_2_year_follow_up_y_arm_1, na.rm = TRUE),
    Alter_SD = sd(age_2_year_follow_up_y_arm_1, na.rm = TRUE)
  )

# Ergebnisse anzeigen
print(sta_t)
print(sta_t_g)

#DHEA

# Deskriptive Statistik berechnen
sta_d <- df_dhea %>%
  summarise(
    N_gesamt = n(),  # Gesamtanzahl der Stichprobe
    N_maennlich = sum(demo_sex_v2 == 1, na.rm = TRUE),  # Anzahl männlich (1)
    N_weiblich = sum(demo_sex_v2 == 2, na.rm = TRUE),  # Anzahl weiblich (2)
    Alter_Mean = mean(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Gesamtmittelwert Alter
    Alter_SD = sd(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Gesamtstandardabweichung Alter
    Alter_Min = min(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Minimales Alter
    Alter_Max = max(age_2_year_follow_up_y_arm_1, na.rm = TRUE)  # Maximales Alter
  )

# Deskriptive Statistik getrennt nach Geschlecht
sta_d_g <- df_dhea %>%
  group_by(demo_sex_v2) %>%
  summarise(
    N = n(),
    Alter_Mean = mean(age_2_year_follow_up_y_arm_1, na.rm = TRUE),
    Alter_SD = sd(age_2_year_follow_up_y_arm_1, na.rm = TRUE)
  )

# Ergebnisse anzeigen
print(sta_d)
print(sta_d_g)

# Estradiol
sta_e <- df_estr %>%
  summarise(
    N_gesamt = n(),  # Gesamtanzahl der Stichprobe
    Alter_Mean = mean(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Mittelwert des Alters
    Alter_SD = sd(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Standardabweichung des Alters
    Alter_Min = min(age_2_year_follow_up_y_arm_1, na.rm = TRUE),  # Minimales Alter
    Alter_Max = max(age_2_year_follow_up_y_arm_1, na.rm = TRUE)  # Maximales Alter
  )
print(sta_e)

########################

####CBCL die klinisch auffälligen bei extexternaliserenden Verhalten raus nehemn (m= ab 17, und w= ab 14)

##Testsoteron männlich 
df_testo_m_nk <- df_testo_m[df_testo_m$cbcl_scr_syn_external_r < 17, ]

##Testosteron weiblich 
df_testo_f_nk <- df_testo_f[df_testo_f$cbcl_scr_syn_external_r < 14, ]

## DHEA männlich 
df_dhea_m_nk <- df_dhea_m[df_dhea_m$cbcl_scr_syn_external_r < 17, ]

## DHEA weiblich 
df_dhea_f_nk <- df_dhea_f[df_dhea_f$cbcl_scr_syn_external_r < 14, ]

## Estradiol weiblich 
## DHEA weiblich 
df_estr_nk <- df_estr[df_estr$cbcl_scr_syn_external_r < 14, ]


#Datensätze standardiseren 
###standardieren (Mittelwert von 0 und eine Standardabweichung von 1)

# Tabelle Testosteron männlich 
df_testo_m_nk_s<- df_testo_m_nk %>%
  mutate(across(c("residuals_test", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_rulebreak_r",
                  "cbcl_scr_syn_external_r", "UPPS", "age_2_year_follow_up_y_arm_1", "age_4_year_follow_up_y_arm_1"), ~ scale(.)[, 1], .names = "{.col}_std"))

# Tabelle Testosteron weiblich 
df_testo_f_nk_s<- df_testo_f_nk %>%
  mutate(across(c("residuals_test", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_rulebreak_r",
                  "cbcl_scr_syn_external_r", "UPPS", "age_2_year_follow_up_y_arm_1", "age_4_year_follow_up_y_arm_1"), ~ scale(.)[, 1], .names = "{.col}_std"))

# Tabelle DHEA männlich  
df_dhea_m_nk_s<- df_dhea_m_nk %>%
  mutate(across(c("residuals_dhea", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_rulebreak_r",
                  "cbcl_scr_syn_external_r", "UPPS", "age_2_year_follow_up_y_arm_1", "age_4_year_follow_up_y_arm_1"), ~ scale(.)[, 1], .names = "{.col}_std"))
# Tabelle DHEA weiblich   
df_dhea_f_nk_s<- df_dhea_f_nk %>%
  mutate(across(c("residuals_dhea", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_rulebreak_r",
                  "cbcl_scr_syn_external_r", "UPPS", "age_2_year_follow_up_y_arm_1","age_4_year_follow_up_y_arm_1"), ~ scale(.)[, 1], .names = "{.col}_std"))
# Tabelle Estradiol weiblich   
df_estr_nk_s<- df_estr_nk %>%
  mutate(across(c("residuals_estr", "cbcl_scr_syn_aggressive_r", "cbcl_scr_syn_rulebreak_r",
                  "cbcl_scr_syn_external_r", "UPPS", "age_2_year_follow_up_y_arm_1", "age_4_year_follow_up_y_arm_1"), ~ scale(.)[, 1], .names = "{.col}_std"))

##nach standardiserung ist ext überall unter 3,5 sd 
# Definiere die Grenzen für Winsorizing
lower_limit <- -3.5  # Untere Grenze bei -3.5 SD
upper_limit <- 3.5   # Obere Grenze bei +3.5 SD

#Testosteron männlich 
df_testo_m_nk_s$test_w <- pmin(pmax(df_testo_m_nk_s$residuals_test_std, lower_limit), upper_limit)

# Testosteron weiblich 
df_testo_f_nk_s$test_w <- pmin(pmax(df_testo_f_nk_s$residuals_test_std, lower_limit), upper_limit)

#DHEA männlich 
df_dhea_m_nk_s$dhea_w <- pmin(pmax(df_dhea_m_nk_s$residuals_dhea_std, lower_limit), upper_limit)

# DHEA weiblich 
df_dhea_f_nk_s$dhea_w <- pmin(pmax(df_dhea_f_nk_s$residuals_dhea_std, lower_limit), upper_limit)

# Estradiol weiblich 
df_estr_nk_s$estr_w <- pmin(pmax(df_estr_nk_s$residuals_estr_std, lower_limit), upper_limit)

#####################################################

### Scatterplot für Testosteron 
ggplot() +
  # Punkte und Regressionslinie für männlich
  geom_point(data = df_testo_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Männlich"), alpha = 0.7, size = 3) +
  geom_smooth(data = df_testo_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Männlich", fill = "Männlich"),
              method = "lm", se = TRUE, size = 1) +
  
  # Punkte und Regressionslinie für weiblich
  geom_point(data = df_testo_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Weiblich"), alpha = 0.7, size = 3) +
  geom_smooth(data = df_testo_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Weiblich", fill = "Weiblich"),
              method = "lm", se = TRUE, size = 1) +
  
  # Null-Linie hinzufügen und hervorheben
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Farben für Punkte, Linien und CIs festlegen
  scale_color_manual(values = c("Männlich" = "blue", "Weiblich" = "red")) +
  scale_fill_manual(values = c("Männlich" = "blue", "Weiblich" = "red"), guide = "none") +
  
  # Labels und Titel
  labs(
    title = "Zusammenhang zwischen Testosteron und externalisiendem Verhalten",
    x = "externalisierendes Verhalten",
    y = "Testosteron",
    color = "Geschlecht"  # Legende für Farben
  ) +
  
  # APA-konformes Theme
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Zentrierter Titel
    axis.title = element_text(size = 12),  # Achsentitel
    axis.text = element_text(size = 10),  # Achsentext
    legend.position = "top",  # Legende oben
    legend.title = element_text(size = 12, face = "bold"),  # Legendentitel
    legend.text = element_text(size = 10)  # Legendentext
  )


### Testsoteron 
#### PLot mit robuster korrelation mit CI 
## Abbildung 10
library(ggplot2)
library(MASS)

# Robuste lineare Modelle für männlich und weiblich erstellen
model_male_test <- rlm(test_w ~ cbcl_scr_syn_external_r_std, data = df_testo_m_nk_s)
model_female_test <- rlm(test_w ~ cbcl_scr_syn_external_r_std, data = df_testo_f_nk_s)

# Funktion zur Berechnung des Konfidenzintervalls
get_ci <- function(model, x_values) {
  coefs <- coef(model)  # Koeffizienten der robusten Regression
  vcov_mat <- vcov(model)  # Kovarianzmatrix der Koeffizienten
  se <- sqrt(diag(vcov_mat))  # Standardfehler der Koeffizienten
  
  # Berechnung der Fits und der Konfidenzgrenzen
  y_fit <- coefs[1] + coefs[2] * x_values
  ci_upper <- y_fit + 1.96 * sqrt(se[1]^2 + (x_values * se[2])^2)
  ci_lower <- y_fit - 1.96 * sqrt(se[1]^2 + (x_values * se[2])^2)
  
  # Datenframe mit x-Werten, Fits und Konfidenzintervallen
  data.frame(x = x_values, y = y_fit, ci_upper = ci_upper, ci_lower = ci_lower)
}

# Konfidenzintervalle für männlich berechnen
x_values_male <- seq(min(df_testo_m_nk_s$cbcl_scr_syn_external_r_std), 
                     max(df_testo_m_nk_s$cbcl_scr_syn_external_r_std), 
                     length.out = 100)
ci_male <- get_ci(model_male_test, x_values_male)

# Konfidenzintervalle für weiblich berechnen
x_values_female <- seq(min(df_testo_f_nk_s$cbcl_scr_syn_external_r_std), 
                       max(df_testo_f_nk_s$cbcl_scr_syn_external_r_std), 
                       length.out = 100)
ci_female <- get_ci(model_female_test, x_values_female)

# Scatterplot mit robusten Regressionslinien und Konfidenzintervallen
ggplot() +
  # Punkte für männlich
  geom_point(data = df_testo_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Männlich"), alpha = 0.7, size = 3) +
  
  # Robuste Regressionslinie für männlich
  geom_line(data = ci_male, aes(x = x, y = y, color = "Männlich"), size = 1) +
  # Konfidenzintervall für männlich
  geom_ribbon(data = ci_male, aes(x = x, ymin = ci_lower, ymax = ci_upper, fill = "Männlich"), alpha = 0.2) +
  
  # Punkte für weiblich
  geom_point(data = df_testo_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Weiblich"), alpha = 0.7, size = 3) +
  
  # Robuste Regressionslinie für weiblich
  geom_line(data = ci_female, aes(x = x, y = y, color = "Weiblich"), size = 1) +
  # Konfidenzintervall für weiblich
  geom_ribbon(data = ci_female, aes(x = x, ymin = ci_lower, ymax = ci_upper, fill = "Weiblich"), alpha = 0.2) +
  
  # Null-Linie hinzufügen und hervorheben
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Farben für Punkte, Linien und Konfidenzintervalle
  scale_color_manual(values = c("Männlich" = "blue", "Weiblich" = "red")) +
  scale_fill_manual(values = c("Männlich" = "blue", "Weiblich" = "red"), guide = "none") +
  
  # Labels und Titel
  labs(
    title = "Zusammenhang zwischen Testosteron und externalisierendem Verhalten (Robuste Regression)",
    x = "externalisierendes Verhalten",
    y = "Testosteron",
    color = "Geschlecht"  # Legende für Farben
  ) +
  
  # APA-konformes Theme
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Zentrierter Titel
    axis.title = element_text(size = 12),  # Achsentitel
    axis.text = element_text(size = 10),  # Achsentext
    legend.position = "top",  # Legende oben
    legend.title = element_text(size = 12, face = "bold"),  # Legendentitel
    legend.text = element_text(size = 10)  # Legendentext
  )




## DHEA 
## Abbildung 11

library(ggplot2)
library(MASS)

# Robuste lineare Modelle für männlich und weiblich erstellen
model_male_dhea <- rlm(dhea_w ~ cbcl_scr_syn_external_r_std, data = df_dhea_m_nk_s)
model_female_dhea <- rlm(dhea_w ~ cbcl_scr_syn_external_r_std, data = df_dhea_f_nk_s)

# Funktion zur Berechnung des Konfidenzintervalls
get_ci <- function(model, x_values) {
  coefs <- coef(model)  # Koeffizienten der robusten Regression
  vcov_mat <- vcov(model)  # Kovarianzmatrix der Koeffizienten
  se <- sqrt(diag(vcov_mat))  # Standardfehler der Koeffizienten
  
  # Berechnung der Fits und der Konfidenzgrenzen
  y_fit <- coefs[1] + coefs[2] * x_values
  ci_upper <- y_fit + 1.96 * sqrt(se[1]^2 + (x_values * se[2])^2)
  ci_lower <- y_fit - 1.96 * sqrt(se[1]^2 + (x_values * se[2])^2)
  
  # Datenframe mit x-Werten, Fits und Konfidenzintervallen
  data.frame(x = x_values, y = y_fit, ci_upper = ci_upper, ci_lower = ci_lower)
}

# Konfidenzintervalle für männlich berechnen
x_values_male <- seq(min(df_dhea_m_nk_s$cbcl_scr_syn_external_r_std), 
                     max(df_dhea_m_nk_s$cbcl_scr_syn_external_r_std), 
                     length.out = 100)
ci_male <- get_ci(model_male_dhea, x_values_male)

# Konfidenzintervalle für weiblich berechnen
x_values_female <- seq(min(df_dhea_f_nk_s$cbcl_scr_syn_external_r_std), 
                       max(df_dhea_f_nk_s$cbcl_scr_syn_external_r_std), 
                       length.out = 100)
ci_female <- get_ci(model_female_dhea, x_values_female)

# Scatterplot mit robusten Regressionslinien und Konfidenzintervallen
ggplot() +
  # Punkte für männlich
  geom_point(data = df_dhea_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = dhea_w, color = "Männlich"), alpha = 0.7, size = 3) +
  
  # Robuste Regressionslinie für männlich
  geom_line(data = ci_male, aes(x = x, y = y, color = "Männlich"), size = 1) +
  # Konfidenzintervall für männlich
  geom_ribbon(data = ci_male, aes(x = x, ymin = ci_lower, ymax = ci_upper, fill = "Männlich"), alpha = 0.2) +
  
  # Punkte für weiblich
  geom_point(data = df_dhea_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = dhea_w, color = "Weiblich"), alpha = 0.7, size = 3) +
  
  # Robuste Regressionslinie für weiblich
  geom_line(data = ci_female, aes(x = x, y = y, color = "Weiblich"), size = 1) +
  # Konfidenzintervall für weiblich
  geom_ribbon(data = ci_female, aes(x = x, ymin = ci_lower, ymax = ci_upper, fill = "Weiblich"), alpha = 0.2) +
  
  # Null-Linie hinzufügen und hervorheben
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Farben für Punkte, Linien und Konfidenzintervalle
  scale_color_manual(values = c("Männlich" = "blue", "Weiblich" = "red")) +
  scale_fill_manual(values = c("Männlich" = "blue", "Weiblich" = "red"), guide = "none") +
  
  # Labels und Titel
  labs(
    title = "Zusammenhang zwischen DHEA und externalisierendem Verhalten (Robuste Regression)",
    x = "externalisierendes Verhalten",
    y = "DHEA",
    color = "Geschlecht"  # Legende für Farben
  ) +
  
  # APA-konformes Theme
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Zentrierter Titel
    axis.title = element_text(size = 12),  # Achsentitel
    axis.text = element_text(size = 10),  # Achsentext
    legend.position = "top",  # Legende oben
    legend.title = element_text(size = 12, face = "bold"),  # Legendentitel
    legend.text = element_text(size = 10)  # Legendentext
  )



#Estradiol
# Abbildung 12

#### mit CI 
library(ggplot2)
library(MASS)

# Robustes lineares Modell für weiblich erstellen
model_female_estr <- rlm(estr_w ~ cbcl_scr_syn_external_r_std, data = df_estr_nk_s)

# Funktion zur Berechnung des Konfidenzintervalls
get_ci <- function(model, x_values) {
  coefs <- coef(model)  # Koeffizienten der robusten Regression
  vcov_mat <- vcov(model)  # Kovarianzmatrix der Koeffizienten
  se <- sqrt(diag(vcov_mat))  # Standardfehler der Koeffizienten
  
  # Berechnung der Fits und der Konfidenzgrenzen
  y_fit <- coefs[1] + coefs[2] * x_values
  ci_upper <- y_fit + 1.96 * sqrt(se[1]^2 + (x_values * se[2])^2)
  ci_lower <- y_fit - 1.96 * sqrt(se[1]^2 + (x_values * se[2])^2)
  
  # Datenframe mit x-Werten, Fits und Konfidenzintervallen
  data.frame(x = x_values, y = y_fit, ci_upper = ci_upper, ci_lower = ci_lower)
}

# Konfidenzintervalle für weiblich berechnen
x_values_female <- seq(min(df_estr_nk_s$cbcl_scr_syn_external_r_std), 
                       max(df_estr_nk_s$cbcl_scr_syn_external_r_std), 
                       length.out = 100)
ci_female <- get_ci(model_female_estr, x_values_female)

# Scatterplot mit robuster Regressionslinie und Konfidenzintervall
ggplot() +
  # Punkte für weiblich
  geom_point(data = df_estr_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = estr_w, color = "Weiblich"), alpha = 0.7, size = 3) +
  
  # Robuste Regressionslinie für weiblich
  geom_line(data = ci_female, aes(x = x, y = y, color = "Weiblich"), size = 1) +
  # Konfidenzintervall für weiblich
  geom_ribbon(data = ci_female, aes(x = x, ymin = ci_lower, ymax = ci_upper, fill = "Weiblich"), alpha = 0.2) +
  
  # Null-Linie hinzufügen und hervorheben
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Farben für Punkte, Linie und Konfidenzintervall
  scale_color_manual(values = c("Weiblich" = "red")) +
  scale_fill_manual(values = c("Weiblich" = "red"), guide = "none") +
  
  # Labels und Titel
  labs(
    title = "Zusammenhang zwischen Estradiol und externalisierendem Verhalten (Robuste Regression)",
    x = "externalisierendes Verhalten",
    y = "Estradiol",
    color = "Geschlecht"  # Legende für Farben
  ) +
  
  # APA-konformes Theme
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Zentrierter Titel
    axis.title = element_text(size = 12),  # Achsentitel
    axis.text = element_text(size = 10),  # Achsentext
    legend.position = "top",  # Legende oben
    legend.title = element_text(size = 12, face = "bold"),  # Legendentitel
    legend.text = element_text(size = 10)  # Legendentext
  )




# Histogramme für jede Variable erstellen 
# Anhang E
library(ggplot2)
library(patchwork)

# Funktion zum Erstellen der Histogramme mit Normalverteilungskurve und neuen Farben
create_histograms <- function(data, var1_name, var2_name, dataset_name, x_label1, x_label2) {
  # Histogramm für die erste Variable mit Normalverteilungskurve
  p1 <- ggplot(data, aes_string(x = var1_name)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "forestgreen", bins = 20, alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = mean(data[[var1_name]], na.rm = TRUE), 
                                           sd = sd(data[[var1_name]], na.rm = TRUE)), 
                  color = "black", size = 1, linetype = "dashed") +
    labs(title = dataset_name, x = x_label1, y = "Dichte") +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),  # APA-Schriftart
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Zentrierter Titel
      axis.title = element_text(size = 12),  # Achsentitel
      axis.text = element_text(size = 10),  # Achsentext
      panel.grid = element_blank()  # Entfernt Gitterlinien für Klarheit
    )
  
  # Histogramm für die zweite Variable mit Normalverteilungskurve
  p2 <- ggplot(data, aes_string(x = var2_name)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "orange", bins = 20, alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = mean(data[[var2_name]], na.rm = TRUE), 
                                           sd = sd(data[[var2_name]], na.rm = TRUE)), 
                  color = "black", size = 1, linetype = "dashed") +
    labs(title = "", x = x_label2, y = "Dichte") +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),  # APA-Schriftart
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Zentrierter Titel
      axis.title = element_text(size = 12),  # Achsentitel
      axis.text = element_text(size = 10),  # Achsentext
      panel.grid = element_blank()  # Entfernt Gitterlinien für Klarheit
    )
  
  # Kombiniere die beiden Plots untereinander
  p1 / p2
}

# Histogramme für jeden Datensatz erstellen mit benannten Achsentiteln und neuen Farben
plot1 <- create_histograms(df_testo_m_nk_s, 
                           "test_w", "cbcl_scr_syn_external_r_std", 
                           "Datensatz Testosteron (männlich)", 
                           "Testosteron", 
                           "externalisierendes Verhalten")

plot2 <- create_histograms(df_testo_f_nk_s, 
                           "test_w", "cbcl_scr_syn_external_r_std", 
                           "Datensatz Testosteron (weiblich)", 
                           "Testosteron", 
                           "externalisierendes Verhalten")

plot3 <- create_histograms(df_dhea_m_nk_s, 
                           "dhea_w", "cbcl_scr_syn_external_r_std", 
                           "Datensatz DHEA (männlich)", 
                           "DHEA", 
                           "externalisierendes Verhalten")

plot4 <- create_histograms(df_dhea_f_nk_s, 
                           "dhea_w", "cbcl_scr_syn_external_r_std", 
                           "Datensatz DHEA (weiblich)", 
                           "DHEA", 
                           "externalisierendes Verhalten")

plot5 <- create_histograms(df_estr_nk_s, 
                           "estr_w", "cbcl_scr_syn_external_r_std", 
                           "Datensatz Estradiol (weiblich)", 
                           "Estradiol", 
                           "externalisierendes Verhalten")

# Kombiniere alle Plots nebeneinander und füge Trennlinien hinzu
final_plot <- (plot1 + plot_layout(tag_level = "new")) | 
  (plot2 + plot_layout(tag_level = "new")) | 
  (plot3 + plot_layout(tag_level = "new")) | 
  (plot4 + plot_layout(tag_level = "new")) | 
  (plot5 + plot_layout(tag_level = "new")) +
  plot_annotation(tag_levels = "A", theme = theme(plot.tag.position = "top"))

# Zeige den kombinierten Plot
print(final_plot)




##### Robuste Regressionen rechnen 
#mit mauell generierten P-Werten
#Tabelle 3

library(MASS)

#Robustes Modell mit rlm
modell_test_m_ext_nk_ageT3_r_p <- rlm(cbcl_scr_syn_external_r_std ~ test_w + age_2_year_follow_up_y_arm_1_std, data = df_testo_m_nk_s)

# Zusammenfassung des Modells
summary_rlm <- summary(modell_test_m_ext_nk_ageT3_r_p)

# Extrahiere Koeffizienten und Standardfehler
coefficients <- summary_rlm$coefficients
t_values <- coefficients[, "Value"] / coefficients[, "Std. Error"]

# Freiheitsgrade
df <- nrow(df_testo_m_nk_s) - length(coefficients[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values <- 2 * pt(-abs(t_values), df = df)

# Kombiniere die Ergebnisse in einer Tabelle
results <- cbind(coefficients, "t value" = t_values, "p value" = p_values)

# Ergebnisse anzeigen
results


###Testosteron weiblich 
library(MASS)

# Robustes Modell mit rlm
modell_test_f_ext_nk_ageT3_r_p <- rlm(cbcl_scr_syn_external_r_std ~ test_w + age_2_year_follow_up_y_arm_1_std, data = df_testo_f_nk_s)

# Zusammenfassung des Modells
summary_rlm <- summary(modell_test_f_ext_nk_ageT3_r_p)

# Extrahiere Koeffizienten und Standardfehler
coefficients <- summary_rlm$coefficients
t_values <- coefficients[, "Value"] / coefficients[, "Std. Error"]

# Freiheitsgrade
df <- nrow(df_testo_f_nk_s) - length(coefficients[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values <- 2 * pt(-abs(t_values), df = df)

# Kombiniere die Ergebnisse in einer Tabelle
results <- cbind(coefficients, "t value" = t_values, "p value" = p_values)

# Ergebnisse anzeigen
results


## Dhea männlich 
library(MASS)

# Robustes Modell mit rlm
modell_dhea_m_ext_nk_ageT3_r_p <- rlm(cbcl_scr_syn_external_r_std ~ dhea_w + age_2_year_follow_up_y_arm_1_std, data = df_dhea_m_nk_s)

# Zusammenfassung des Modells
summary_rlm <- summary(modell_dhea_m_ext_nk_ageT3_r_p)

# Extrahiere Koeffizienten und Standardfehler
coefficients <- summary_rlm$coefficients
t_values <- coefficients[, "Value"] / coefficients[, "Std. Error"]

# Freiheitsgrade
df <- nrow(df_dhea_m_nk_s) - length(coefficients[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values <- 2 * pt(-abs(t_values), df = df)

# Kombiniere die Ergebnisse in einer Tabelle
results <- cbind(coefficients, "t value" = t_values, "p value" = p_values)

# Ergebnisse anzeigen
results


### ## Dhea weiblich 
library(MASS)

# Robustes Modell mit rlm
modell_dhea_f_ext_nk_ageT3_r_p <- rlm(cbcl_scr_syn_external_r_std ~ dhea_w + age_2_year_follow_up_y_arm_1_std, data = df_dhea_f_nk_s)

# Zusammenfassung des Modells
summary_rlm <- summary(modell_dhea_f_ext_nk_ageT3_r_p)

# Extrahiere Koeffizienten und Standardfehler
coefficients <- summary_rlm$coefficients
t_values <- coefficients[, "Value"] / coefficients[, "Std. Error"]

# Freiheitsgrade
df <- nrow(df_dhea_f_nk_s) - length(coefficients[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values <- 2 * pt(-abs(t_values), df = df)

# Kombiniere die Ergebnisse in einer Tabelle
results <- cbind(coefficients, "t value" = t_values, "p value" = p_values)

# Ergebnisse anzeigen
results


### Estradiol 
library(MASS)

# Robustes Modell mit rlm
modell_estr_ext_nk_ageT3_r_p <- rlm(cbcl_scr_syn_external_r_std ~ estr_w + age_2_year_follow_up_y_arm_1_std, data = df_estr_nk_s)

# Zusammenfassung des Modells
summary_rlm <- summary(modell_estr_ext_nk_ageT3_r_p)

# Extrahiere Koeffizienten und Standardfehler
coefficients <- summary_rlm$coefficients
t_values <- coefficients[, "Value"] / coefficients[, "Std. Error"]

# Freiheitsgrade
df <- nrow(df_estr_nk_s) - length(coefficients[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values <- 2 * pt(-abs(t_values), df = df)

# Kombiniere die Ergebnisse in einer Tabelle
results <- cbind(coefficients, "t value" = t_values, "p value" = p_values)

# Ergebnisse anzeigen
results


#####################################################

####### überprüfen auf nicht linearer-Zusammenahng 
install.packages("mgcv")  
library(mgcv)

#Testsoteorn
## Testo m
gam_model_test_m <- gam(cbcl_scr_syn_external_r_std ~ s(test_w) + s(age_2_year_follow_up_y_arm_1_std), 
                        data = df_testo_m_nk_s, 
                        family = gaussian(link = "identity"))

summary(gam_model_test_m)



#testo w
gam_model_test_f <- gam(cbcl_scr_syn_external_r_std ~ s(test_w) + s(age_2_year_follow_up_y_arm_1_std), 
                        data = df_testo_f_nk_s, 
                        family = gaussian(link = "identity"))

summary(gam_model_test_f)

### Plotten des nicht-linearen Modells: 
plot(gam_model_test_f, pages = 1, shade = TRUE, rug = TRUE, main = "Nicht-lineare Effekte im GAM (Testsoteron weiblich")
 

### DHEA 
#DHEA m 
gam_model_dhea_m <- gam(cbcl_scr_syn_external_r_std ~ s(dhea_w) + s(age_2_year_follow_up_y_arm_1_std), 
                        data = df_dhea_m_nk_s, 
                        family = gaussian(link = "identity"))

summary(gam_model_dhea_m)

plot(gam_model_dhea_m, pages = 1, shade = TRUE, rug = TRUE, main = "Nicht-lineare Effekte im GAM (DHEA männlich)")


#DHEA w
gam_model_dhea_f <- gam(cbcl_scr_syn_external_r_std ~ s(dhea_w) + s(age_2_year_follow_up_y_arm_1_std), 
                        data = df_dhea_f_nk_s, 
                        family = gaussian(link = "identity"))

summary(gam_model_dhea_f)


plot(gam_model_dhea_f, pages = 1, shade = TRUE, rug = TRUE, main = "Nicht-lineare Effekte im GAM (DHEA weiblich)")


## Estradiol
gam_model_estr <- gam(cbcl_scr_syn_external_r_std ~ s(estr_w) + s(age_2_year_follow_up_y_arm_1_std), 
                      data = df_estr_nk_s, 
                      family = gaussian(link = "identity"))

summary(gam_model_estr)

################################################################

### Auf quadratische Beziehungen testen 
#Testoteron 
# Testosteron männlich  
# Robustes Modell mit rlm (mit quadratischem Term)
modell_testo_m_ext_nk_ageT3_r_p_quad <- rlm(cbcl_scr_syn_external_r_std ~ test_w + I(test_w^2) + age_2_year_follow_up_y_arm_1_std, 
                                            data = df_testo_m_nk_s)

# Zusammenfassung des Modells
summary_rlm_quad <- summary(modell_testo_m_ext_nk_ageT3_r_p_quad)

# Extrahiere Koeffizienten und Standardfehler
coefficients_quad <- summary_rlm_quad$coefficients
t_values_quad <- coefficients_quad[, "Value"] / coefficients_quad[, "Std. Error"]

# Freiheitsgrade berechnen
df_quad <- nrow(df_testo_m_nk_s) - length(coefficients_quad[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values_quad <- 2 * pt(-abs(t_values_quad), df = df_quad)

# Kombiniere die Ergebnisse in einer Tabelle
results_quad <- cbind(coefficients_quad, "t value" = t_values_quad, "p value" = p_values_quad)

# Ergebnisse anzeigen
results_quad



# Testoteron weiblich 
# Robustes Modell mit rlm (mit quadratischem Term)
modell_testo_f_ext_nk_ageT3_r_p_quad <- rlm(cbcl_scr_syn_external_r_std ~ test_w + I(test_w^2) + age_2_year_follow_up_y_arm_1_std, 
                                            data = df_testo_f_nk_s)

# Zusammenfassung des Modells
summary_rlm_quad <- summary(modell_testo_f_ext_nk_ageT3_r_p_quad)

# Extrahiere Koeffizienten und Standardfehler
coefficients_quad <- summary_rlm_quad$coefficients
t_values_quad <- coefficients_quad[, "Value"] / coefficients_quad[, "Std. Error"]

# Freiheitsgrade berechnen
df_quad <- nrow(df_testo_f_nk_s) - length(coefficients_quad[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values_quad <- 2 * pt(-abs(t_values_quad), df = df_quad)

# Kombiniere die Ergebnisse in einer Tabelle
results_quad <- cbind(coefficients_quad, "t value" = t_values_quad, "p value" = p_values_quad)

# Ergebnisse anzeigen
results_quad



# DHEA männlich  
# Robustes Modell mit rlm (mit quadratischem Term)
modell_dhea_m_ext_nk_ageT3_r_p_quad <- rlm(cbcl_scr_syn_external_r_std ~ dhea_w + I(dhea_w^2) + age_2_year_follow_up_y_arm_1_std, 
                                           data = df_dhea_m_nk_s)

# Zusammenfassung des Modells
summary_rlm_quad <- summary(modell_dhea_m_ext_nk_ageT3_r_p_quad)

# Extrahiere Koeffizienten und Standardfehler
coefficients_quad <- summary_rlm_quad$coefficients
t_values_quad <- coefficients_quad[, "Value"] / coefficients_quad[, "Std. Error"]

# Freiheitsgrade berechnen
df_quad <- nrow(df_dhea_m_nk_s) - length(coefficients_quad[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values_quad <- 2 * pt(-abs(t_values_quad), df = df_quad)

# Kombiniere die Ergebnisse in einer Tabelle
results_quad <- cbind(coefficients_quad, "t value" = t_values_quad, "p value" = p_values_quad)

# Ergebnisse anzeigen
results_quad



# DHEA weiblich 
# Robustes Modell mit rlm (mit quadratischem Term)
modell_dhea_f_ext_nk_ageT3_r_p_quad <- rlm(cbcl_scr_syn_external_r_std ~ dhea_w + I(dhea_w^2) + age_2_year_follow_up_y_arm_1_std, 
                                           data = df_dhea_f_nk_s)

# Zusammenfassung des Modells
summary_rlm_quad <- summary(modell_dhea_f_ext_nk_ageT3_r_p_quad)

# Extrahiere Koeffizienten und Standardfehler
coefficients_quad <- summary_rlm_quad$coefficients
t_values_quad <- coefficients_quad[, "Value"] / coefficients_quad[, "Std. Error"]

# Freiheitsgrade berechnen
df_quad <- nrow(df_dhea_f_nk_s) - length(coefficients_quad[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values_quad <- 2 * pt(-abs(t_values_quad), df = df_quad)

# Kombiniere die Ergebnisse in einer Tabelle
results_quad <- cbind(coefficients_quad, "t value" = t_values_quad, "p value" = p_values_quad)

# Ergebnisse anzeigen
results_quad

# DHEA weiblich 
# Robustes Modell mit rlm (nur quadratischer Term)
modell_dhea_f_ext_nk_ageT3_r_p_quad_only <- rlm(cbcl_scr_syn_external_r_std ~ I(dhea_w^2) + age_2_year_follow_up_y_arm_1_std, 
                                                data = df_dhea_f_nk_s)

# Zusammenfassung des Modells
summary_rlm_quad_only <- summary(modell_dhea_f_ext_nk_ageT3_r_p_quad_only)

# Extrahiere Koeffizienten und Standardfehler
coefficients_quad_only <- summary_rlm_quad_only$coefficients
t_values_quad_only <- coefficients_quad_only[, "Value"] / coefficients_quad_only[, "Std. Error"]

# Freiheitsgrade berechnen
df_quad_only <- nrow(df_dhea_f_nk_s) - length(coefficients_quad_only[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values_quad_only <- 2 * pt(-abs(t_values_quad_only), df = df_quad_only)

# Kombiniere die Ergebnisse in einer Tabelle
results_quad_only <- cbind(coefficients_quad_only, "t value" = t_values_quad_only, "p value" = p_values_quad_only)

# Ergebnisse anzeigen
results_quad_only


# Estradiol
# Robustes Modell mit rlm (mit quadratischem Term)
modell_estr_ext_nk_ageT3_r_p_quad <- rlm(cbcl_scr_syn_external_r_std ~ estr_w + I(estr_w^2) + age_2_year_follow_up_y_arm_1_std, 
                                         data = df_estr_nk_s)

# Zusammenfassung des Modells
summary_rlm_quad <- summary(modell_estr_ext_nk_ageT3_r_p_quad)

# Extrahiere Koeffizienten und Standardfehler
coefficients_quad <- summary_rlm_quad$coefficients
t_values_quad <- coefficients_quad[, "Value"] / coefficients_quad[, "Std. Error"]

# Freiheitsgrade berechnen
df_quad <- nrow(df_estr_nk_s) - length(coefficients_quad[, "Value"])

# Berechne p-Werte (zweiseitig)
p_values_quad <- 2 * pt(-abs(t_values_quad), df = df_quad)

# Kombiniere die Ergebnisse in einer Tabelle
results_quad <- cbind(coefficients_quad, "t value" = t_values_quad, "p value" = p_values_quad)

# Ergebnisse anzeigen
results_quad

################################################
## Anhang G, Abbildung A
#### Testosteron männlich robuste Regression & weiblich loess 
## weil m linear und w nicht linear aber auch nicht quadratisch 
library(ggplot2)
library(MASS) 

# Robuste lineare Regression für Männer
model_male_testo <- rlm(test_w ~ cbcl_scr_syn_external_r_std, data = df_testo_m_nk_s)

# Vorhersagen für die robuste Regression berechnen
x_values_male <- seq(min(df_testo_m_nk_s$cbcl_scr_syn_external_r_std), 
                     max(df_testo_m_nk_s$cbcl_scr_syn_external_r_std), 
                     length.out = 100)

# Berechnung der Fit-Linie und des Konfidenzintervalls
y_fit_male <- predict(model_male_testo, newdata = data.frame(cbcl_scr_syn_external_r_std = x_values_male), se.fit = TRUE)
ci_male <- data.frame(
  x = x_values_male,
  y = y_fit_male$fit,
  ci_lower = y_fit_male$fit - 1.96 * y_fit_male$se.fit,
  ci_upper = y_fit_male$fit + 1.96 * y_fit_male$se.fit
)

# Plot
ggplot() +
  # Punkte für männlich
  geom_point(data = df_testo_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Männlich"), alpha = 0.7, size = 3) +
  
  # Robuste lineare Regression für männlich
  geom_line(data = ci_male, aes(x = x, y = y, color = "Männlich"), size = 1) +
  geom_ribbon(data = ci_male, aes(x = x, ymin = ci_lower, ymax = ci_upper, fill = "Männlich"), alpha = 0.2, show.legend = FALSE) +
  
  # Punkte für weiblich
  geom_point(data = df_testo_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Weiblich"), alpha = 0.7, size = 3) +
  
  # Nicht-lineare LOESS-Kurve für weiblich
  geom_smooth(data = df_testo_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = test_w, color = "Weiblich", fill = "Weiblich"), 
              method = "loess", se = TRUE, size = 1, alpha = 0.2, show.legend = FALSE) +
  
  # Null-Linie hinzufügen
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Farben für Punkte, Linien & Flächen
  scale_color_manual(values = c("Männlich" = "blue", "Weiblich" = "red")) +
  scale_fill_manual(values = c("Männlich" = "blue", "Weiblich" = "red")) +
  
  # Labels & Titel
  labs(
    title = "Zusammenhang zwischen Testosteron und externalisierendem Verhalten",
    subtitle = "Männlich: Robuste lineare Regression | Weiblich: LOESS-Kurve",
    x = "Externalisierendes Verhalten",
    y = "Testosteron",
    color = "Geschlecht"
  ) +
  
  # APA-konformes Theme
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

## Anahng G, Abbildung B 
#DHEA
library(ggplot2)
library(MASS)

# Robuste quadratische Regression für Frauen
model_female_dhea <- rlm(dhea_w ~ cbcl_scr_syn_external_r_std + I(cbcl_scr_syn_external_r_std^2), data = df_dhea_f_nk_s)

# Werte für die Vorhersage der robusten quadratischen Regression
x_values_female <- seq(min(df_dhea_f_nk_s$cbcl_scr_syn_external_r_std), 
                       max(df_dhea_f_nk_s$cbcl_scr_syn_external_r_std), 
                       length.out = 100)

# Berechnung der Fit-Linie und des Konfidenzintervalls
y_fit_female <- predict(model_female_dhea, newdata = data.frame(cbcl_scr_syn_external_r_std = x_values_female, 
                                                                I(cbcl_scr_syn_external_r_std^2) = x_values_female^2), 
                        se.fit = TRUE)

ci_female <- data.frame(
  x = x_values_female,
  y = y_fit_female$fit,
  ci_lower = y_fit_female$fit - 1.96 * y_fit_female$se.fit,
  ci_upper = y_fit_female$fit + 1.96 * y_fit_female$se.fit
)

#  Plot
ggplot() +
  # Punkte für männlich
  geom_point(data = df_dhea_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = dhea_w, color = "Männlich"), alpha = 0.7, size = 3) +
  
  # Nicht-lineare LOESS-Kurve für männlich mit blauem CI
  geom_smooth(data = df_dhea_m_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = dhea_w, color = "Männlich"), 
              method = "loess", se = TRUE, size = 1, alpha = 0.2, fill = "blue", show.legend = FALSE) +
  
  # Punkte für weiblich
  geom_point(data = df_dhea_f_nk_s, aes(x = cbcl_scr_syn_external_r_std, y = dhea_w, color = "Weiblich"), alpha = 0.7, size = 3) +
  
  # Robuste quadratische Regression für weiblich
  geom_line(data = ci_female, aes(x = x, y = y, color = "Weiblich"), size = 1) +
  
  # Konfidenzintervall für weiblich entfernen
  geom_ribbon(data = ci_female, aes(x = x, ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.2, show.legend = FALSE) +
  
  # Null-Linie hinzufügen
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Farben für Punkte & Linien
  scale_color_manual(values = c("Männlich" = "blue", "Weiblich" = "red")) +
  
  # Labels & Titel
  labs(
    title = "Zusammenhang zwischen DHEA und externalisierendem Verhalten",
    subtitle = "Männlich: LOESS-Kurve | Weiblich: Robuste quadratische Regression",
    x = "Externalisierendes Verhalten",
    y = "DHEA",
    color = "Geschlecht"
  ) +
  
  # APA-konformes Theme
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )