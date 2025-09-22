library(deSolve)

run_ode <- function(init, seir_fn, times, parms){
  df <- ode(y=init, func = seir_fn, times=times, parms = parms) %>%
    as.data.frame() %>% as_tibble() %>% pivot_longer(-time) %>%
    mutate(age_group = case_when(
      grepl("^S_C|^E_C|^I_C|^H_C|^R_C|^D_C", name) ~ "Children",
      grepl("^S_OC|^E_OC|^I_OC|^H_OC|^R_OC|^D_OC", name) ~ "Older Children",
      grepl("^S_A|^E_A|^I_A|^H_A|^R_A|^D_A", name) ~ "Adults",
      grepl("^S_S|^E_S|^I_S|^H_S|^R_S|^D_S", name) ~ "Seniors")) %>%
    mutate(disease_group = case_when(
      grepl("RSV", name) ~ "RSV",
      grepl("COV", name) ~ "Covid",
      grepl("FLU", name) ~ "Flu")) %>%
    mutate(type = case_when(
      grepl("^S_", name) ~ "Susceptible",
      grepl("E_", name) ~ "Exposed",
      grepl("I_", name) ~ "Infected",
      grepl("H_", name) ~ "Hospitalized",
      grepl("R_", name) ~ "Recovered",
      grepl("D_", name) ~ "Dead")) %>% 
    mutate(vacc_type = case_when(
      grepl("_vax", name) ~ "Vaccinated",
      TRUE ~ "Unvaccinated")) %>%
    # get percent for overall age group 
    group_by(time, age_group) %>% 
    mutate(total_value = sum(value),
           age_percent = value / total_value) %>%
    ungroup() %>%
    # get percent for age group and vaccination type
    group_by(time, age_group, vacc_type) %>% 
    mutate(total_value = sum(value),
           vacc_percent = value / total_value) %>%
    ungroup()
  df$age_groupf = factor(df$age_group, levels=c("Children", "Older Children", "Adults", "Seniors"))
  return(df)
}

plot_infections_count_by_vacc <- function(df, t, novac){
  if (missing(novac)){
    plot <- df %>% filter(type == "Infected") %>%
      filter(time <= t) %>%
      ggplot(aes(x=time,y=value, color=vacc_type)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) +
      ggtitle("Infections by Vaccination Type (Count)")
  } else {
    plot <- df %>% filter(type == "Infected") %>% 
      filter(vacc_type == "Unvaccinated") %>%
      filter(time <= 400) %>%
      ggplot(aes(x=time,y=value, color=disease_group)) + 
      geom_line() + 
      scale_color_paletteer_d("tvthemes::Amethyst") +
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) + 
      ggtitle(paste("Infections of Unvaccinated Individuals (Count)"))
  }
  return(plot)
}

plot_infections_percent_by_vacc <- function(df, t, novac){
  if (missing(novac)){
    plot <- df %>% filter(type == "Infected") %>%
      filter(time <= t) %>%
      ggplot(aes(x=time, y=age_percent, color=vacc_type)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) +
      ylim(0, .2) +
      # scale_color_paletteer_d("nationalparkcolors::Acadia") +
      labs("Vaccination Type") + 
      ggtitle(paste("Infections by Vaccination Type (Percent)"))
  } else {
    plot <- df %>% filter(type == "Infected") %>% 
      filter(vacc_type == "Unvaccinated") %>%
      filter(time <= 400) %>%
      ggplot(aes(x=time, y=vacc_percent, color=disease_group)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) + 
      ylim(0, .2) + 
      xlab("Time (Days)") +
      ylab("Percent Population") +
      labs(color = "Pathogen") + 
      scale_color_paletteer_d("tvthemes::Amethyst") +
      ggtitle(paste("Infections of Unvaccinated Individuals (Percent)"))
  }
  return(plot)
}

plot_percent_total_infections <- function(df) {
  grouped_df <- df %>% filter(type=="Infected") %>%
    group_by(disease_group, time) %>%
    summarise(total = sum(value))
  grouped_df$percent <- grouped_df$total / PopT
  plot <- grouped_df %>%
    ggplot(aes(x=time, y=percent, color=disease_group)) + 
    geom_line() + 
    ylim(0, .2) + 
    ylab("Percent Population Infected") + 
    xlab("Time (Days)") + 
    labs(color = "Pathogen") +
    scale_color_paletteer_d("tvthemes::Alexandrite") +
    ggtitle(paste("Total Infections by Pathogen (Percent)"))
  return(plot)
}

plot_hosp_count_by_vacc <- function(df, t, novac){
  if (missing(novac)){
    plot <- df %>% filter(type == "Hospitalized") %>%
      filter(time <= t) %>%
      ggplot(aes(x=time,y=value, color=vacc_type)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) +
      geom_hline(yintercept=12334, col="red", linetype="dotted") +
      ggtitle("Hospitalizations by Vaccination Type (Count)")
  } else {
    plot <- df %>% filter(type == "Hospitalized") %>% 
      filter(vacc_type == "Unvaccinated") %>%
      filter(time <= 400) %>%
      ggplot(aes(x=time,y=value)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) + 
      geom_hline(yintercept=12334, col="red", linetype="dotted") +
      ggtitle(paste("Hospitalizations of Unvaccinated Individuals (Count)"))
  }
  return(plot)
}

plot_cum_deaths <- function(df, novac){
  if (!missing(novac)){
    plot <- df %>% filter(type == "Dead") %>%
      filter(vacc_type == "Unvaccinated") %>%
      group_by(time, age_group, disease_group) %>%
      mutate(cum_death = cumsum(value)) %>%
      ungroup() %>%
      ggplot(aes(x=time,y=cum_death, color=disease_group)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE) +
      xlab("Time (Days)") +
      ylab("Cumulative Deaths (Count)") +
      labs(color = "Pathogen") + 
      scale_color_paletteer_d("tvthemes::Amethyst") +
      ggtitle("Cumulative Deaths (Count)")
  } else {
    plot <- df %>% filter(type == "Dead") %>%
      group_by(time, age_group, disease_group, vacc_type) %>%
      mutate(cum_death = cumsum(value)) %>%
      ungroup() %>%
      ggplot(aes(x=time,y=cum_death, color=vacc_type)) + 
      geom_line() + 
      facet_grid(rows=vars(age_groupf), cols=vars(disease_group), drop = FALSE, scales="free") +
      xlab("Time (Days)") +
      ylab("Cumulative Deaths (Count)") +
      labs(color = "Vaccination Type") + 
      ggtitle("Cumulative Deaths (Count)")
  }
  return(plot)
}

save_plot <- function(plot, name, width, height){
  path <- paste0("figures/", name, ".png")
  ggsave(path, plot, width=width, height=height)
}
