# Check industry and sub-industry RFPs


# Check CO2 emissions
var_tot <- "Emissions|CO2|Energy|Demand|Industry"
var_sub <- c("Emissions|CO2|Energy|Demand|Industry|Chemicals|Beta",
             "Emissions|CO2|Energy|Demand|Industry|Steel|Beta", "Emissions|CO2|Energy|Demand|Industry|Cement|Beta",
             "Emissions|CO2|Energy|Demand|Industry|Other|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - CO2 emissions", subtitle = "[MtCO2/yr]")



# Check electricity levels
var_tot <- "Final Energy|Industry|Electricity"
var_sub <- c("Final Energy|Industry|Chemicals|Electricity|Beta",
             "Final Energy|Industry|Cement|Electricity|Beta", 
             "Final Energy|Industry|Steel|Electricity|Beta",
             "Final Energy|Industry|Other|Electricity|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - electricity consumption", subtitle = "[EJ/yr]")


# Check heat levels
var_tot <- "Final Energy|Industry|Heat"
var_sub <- c("Final Energy|Industry|Chemicals|Heat|Beta",
             "Final Energy|Industry|Cement|Heat|Beta", 
             "Final Energy|Industry|Steel|Heat|Beta",
             "Final Energy|Industry|Other|Heat|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - heat consumption", subtitle = "[EJ/yr]")



# Check H2 levels
var_tot <- "Final Energy|Industry|Hydrogen"
var_sub <- c("Final Energy|Industry|Chemicals|Hydrogen|Beta",
             "Final Energy|Industry|Cement|Hydrogen|Beta", 
             "Final Energy|Industry|Steel|Hydrogen|Beta",
             "Final Energy|Industry|Other|Hydrogen|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - H2 consumption", subtitle = "[EJ/yr]")


# Check Gases levels
var_tot <- "Final Energy|Industry|Gases"
var_sub <- c("Final Energy|Industry|Chemicals|Gases|Beta",
             "Final Energy|Industry|Cement|Gases|Beta", 
             "Final Energy|Industry|Steel|Gases|Beta",
             "Final Energy|Industry|Other|Gases|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - Gases consumption", subtitle = "[EJ/yr]")


# Check Liquids levels
var_tot <- "Final Energy|Industry|Liquids"
var_sub <- c("Final Energy|Industry|Chemicals|Liquids|Beta",
             "Final Energy|Industry|Cement|Liquids|Beta", 
             "Final Energy|Industry|Steel|Liquids|Beta",
             "Final Energy|Industry|Other|Liquids|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - Liquids consumption", subtitle = "[EJ/yr]")


# Check Solids levels
var_tot <- "Final Energy|Industry|Solids"
var_sub <- c("Final Energy|Industry|Chemicals|Solids|Beta",
             "Final Energy|Industry|Cement|Solids|Beta", 
             "Final Energy|Industry|Steel|Solids|Beta",
             "Final Energy|Industry|Other|Solids|Beta")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  geom_line(aes(x=period, y=value), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(title="Test - Solids consumption", subtitle = "[EJ/yr]")



# RFP: Low-carbon capital expenditure
var_tot <- "Diagnostics|Investment|Energy Efficiency"
var_sub <- c("Capital|Energy Efficiency|Industry|Chemicals",
             "Capital|Energy Efficiency|Industry|Cement", 
             "Capital|Energy Efficiency|Industry|Steel",
             "Capital|Energy Efficiency|Industry|other")

tmp <- data_rfp_input %>% 
  filter(grepl("REMIND", model), region == "World") %>% 
  filter(variable %in% c(var_tot, var_sub))

ggplot(tmp %>% filter(variable %in% var_sub)) +
  geom_area(aes(x=period, y=value, fill=variable)) +
  #geom_line(aes(x=period, y=value*0.30), lwd=2, data=tmp %>% filter(variable %in% var_tot)) +
  theme_bw() +
  facet_wrap(~scenario) +
  xlab("") + ylab("") + labs(title="Test - CO2 emissions", subtitle = "[MtCO2/yr]")
