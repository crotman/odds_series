
library(tidyverse)
library(uuid)


transform_data <- function(file){


    odds <- read_csv(file)
    
    
    odds_split <- odds %>% 
        mutate_at(
            vars(matches("home|draw|away")),
            function(x){if_else(is.nan(x),NA_real_,x)}
        ) %>% 
        group_by(row_number() %% 16) %>% 
        nest()

    
    
    vira <- function(data){
        
        id <- UUIDgenerate()
        
        pivot_longer(
            data = data,
            cols = home_b1_0:away_b32_71,
            names_pattern = "(.*)_b([0-9]*)_([0-9]*)",
            names_to = c("result", "broker", "time"),
            values_to = "odd",
            values_drop_na = TRUE
        ) %>% 
            group_by(match_id, result, broker) %>% 
            arrange(
                desc(time)
            ) %>% 
            mutate(
                prev_odd = lag(odd)
            ) %>% 
            filter(odd != prev_odd) %>% 
            write_rds(path = str_glue("data/ready/{id}.rds"))
    }
    

    
    walk(.x = odds_split$data, .f = vira )

}    


# transform_data(file = "data/raw/odds_series.csv")
transform_data(file = "data/raw/odds_series_b.csv")







