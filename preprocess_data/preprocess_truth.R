# create json files with truth in them
# files are named as data/truth_<target_var>_<location>_<as_of>.json, where
# target_var is "case", "death", or "hosp",
# location is an identifier for a location, and
# as_of is a date in YYYY-MM-DD format indicating the data version date.
# A single json file has data in the format of a list of (date, value) pairs:
# [
#    {date: "YYYY-MM-DD", value: 0},
#    ...,
#    {date: "YYYY-MM-DD", value: 1}
# ]

library(covidData)
library(tidyverse)
library(jsonlite)
library(sp)

last_as_of <- lubridate::floor_date(Sys.Date(), unit = "week", week_start = 6)

locations_json <- covidData::fips_codes %>%
    dplyr::filter(nchar(location) == 5, !is.na(population)) %>%
    dplyr::select(value = location, text = location_name, population = population) %>%
    jsonlite::toJSON()
writeLines(
    locations_json,
    paste0("static/locations.json")
)

counties <- geojsonio::topojson_read("static/counties-albers-10m.json")

get_geojson_features <- function(x) {
    tf <- tempfile(fileext = ".geojson")
    suppressMessages(sf::st_write(x, tf))
    temp <- jsonlite::fromJSON(tf)
    return(temp$features)
}

for (target_var in c("case", "death")) {
    if (target_var == "death") {
        data_start_date <- as.Date("2020-03-01")
    } else if (target_var == "case") {
        data_start_date <- as.Date("2020-03-01")
    } else {
        data_start_date <- as.Date("2020-10-01")
    }
    data <- covidData::load_data(
        as_of = last_as_of + 1,
        temporal_resolution = "weekly",
        spatial_resolution = c("county"),
        measure = target_var
    )

    locations <- covidData::fips_codes %>%
        dplyr::filter(nchar(location) == 5, !is.na(population)) %>%
        dplyr::pull(location)

    data <- data %>%
        dplyr::filter(location %in% locations)

    data <- data %>%
        dplyr::left_join(
            covidData::fips_codes %>%
                dplyr::filter(nchar(location) == 5) %>%
                dplyr::transmute(
                    location = location,
                    pop100k = population / 100000),
            by = "location"
        ) %>%
        dplyr::mutate(rate = inc / pop100k) %>%
        dplyr::filter(!is.na(rate))

    rate_quantiles <- data %>%
        dplyr::pull(rate) %>%
        quantile(c(0.5, 0.8, 0.95))

    data_json <- purrr::map(
        unique(data$date),
        function(date_val) {
            low_rate_county_ids <- data %>%
                dplyr::filter(
                    date == date_val,
                    rate >= rate_quantiles[1]#,
#                    rate < rate_quantiles[2]
                    ) %>%
                dplyr::pull(location) %>%
                unique()
            low_rate_counties_merged <- counties %>%
                dplyr::filter(id %in% low_rate_county_ids) %>%
                sf::st_union()

            med_rate_county_ids <- data %>%
                dplyr::filter(
                    date == date_val,
                    rate >= rate_quantiles[2]#,
#                    rate < rate_quantiles[3]
                ) %>%
                dplyr::pull(location) %>%
                unique()
            med_rate_counties_merged <- counties %>%
                dplyr::filter(id %in% med_rate_county_ids) %>%
                sf::st_union()

            hi_rate_county_ids <- data %>%
                dplyr::filter(
                    date == date_val,
                    rate >= rate_quantiles[3]) %>%
                dplyr::pull(location) %>%
                unique()
            hi_rate_counties_merged <- counties %>%
                dplyr::filter(id %in% hi_rate_county_ids) %>%
                sf::st_union()

            list(
                "low" = get_geojson_features(low_rate_counties_merged),
                "med" = get_geojson_features(med_rate_counties_merged),
                "high" = get_geojson_features(hi_rate_counties_merged)
            )
        }) %>%
        `names<-`(unique(data$date)) %>%
        jsonlite::toJSON()

    writeLines(
        data_json,
        paste0("static/merged_data_", target_var,  ".json")
    )
}
