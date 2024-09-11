library(castoRedc)
library(DBI)
library(odbc)


get_api_connection <- function(clientkey, clientsecret) {
    #' -- gets the CastoR API connection -- '#
    
    castor_api <- CastorData$new(key = clientkey, secret = clientsecret, base_url = "https://data.castoredc.com")
    
    return(castor_api)
}

get_umbrella_ids <- function(api, study_id) {
    #' -- gets all of the umbrella IDs which are registered -- '#
    return(api$getRecords(study_id)$record_id)
}

get_data_from_umbrella_id <- function(api, study_id, umbrella_id) {
    #' -- gets all of the answers of all questions for a given umbrella ID -- '#

    # get data, tryCatch for when an UMBRELLA ID does exist but does not hold any information
    data <- tryCatch(api$getStudyDataPoints(study_id, umbrella_id), error = function(e) NULL)
    data <- as.data.frame(data)

    return(data)
}

extract_columns <- function(df, col_prefixes) {
    #' -- extracts all of the right questions needed for the calculations of the scores -- '#
    
    patterns <- paste0(col_prefixes, collapse="|")
    df <- df[grepl(patterns, names(df))]
    
    return(df)
}

get_all_data <- function(api, study_id, umbrella_ids, col_prefixes) {
    #' -- get all the data for all the given umbrella IDs and columns                          --'#
    #' -- returns this a list of dfs, where each df contains all the dqta from one umbrella ID --'#
    
    all_data = list()
    for (umbrella_id in umbrella_ids) {
        values <- get_data_from_umbrella_id(api, study_id, umbrella_id)
        values <- extract_columns(values, col_prefixes)
        suppressWarnings(values <- apply(values, 2, as.numeric))  # replaces all non-numeric values with NA (gives NA introduced warning)
        all_data[[umbrella_id]] <- values
    }

    return(all_data)
}

convert_score_to_perc <- function(score, max_score, inverted) {
    #' -- converts a score to a percentage -- '#

    # scoring starts at 1 not 0
    score <- score - 1
    max_score <- max_score - 1  

    # if the highest score indicates the best outcome
    if (!inverted){
        percentage <-  (score / max_score) * 100
    # if the highest score indicates the worst itcome, invert the scores
    } else {
        percentage <- (1 - (score / max_score)) * 100
    }
    

    return(percentage)  
}


calculate_scores <- function(df, id, survey, survey_info) {
    #' -- Calculates the score for each period and domain                             -- '#
    #' -- It calculates the mean score of each domain and converts it to a percentage -- '#
    
    periods <- unique(unlist(lapply(strsplit(names(df), "_"), tail, n=1)))   # get period (C30_26_T12 --> T12)
    periods <- lapply(periods, function(x) sub("^T", "", x))  # remove T at end (T12 --> 12)
    domains <- names(survey)

    # initialize an empty df to add all of the rseults to
    df_scores <- data.frame(matrix(ncol=4, nrow=0))
    colnames(df_scores) <- c("umbrella_id", "domain", "period", "score")
    
    for (period in periods) {
        for (domain in domains) {

            questions <- paste0(survey[[domain]], period)

            # the domain Sexueel functioneren got an extra option "Zeg ik liever niet"
            # This got value 5, however this can be seen as missing and not a higher score
            if (domain == "Sexueel functioneren"){
                df[questions][df[questions] == 5] <- NA
            }

            # calculation of the scores
            mean_score <-  mean(df[questions], na.rm=TRUE)
            score <- convert_score_to_perc(mean_score, survey_info[[domain]]$max_score,  survey_info[[domain]]$inverted)

            # add to dataframe
            df_scores[nrow(df_scores)+1,] <-list(id, domain, period, score)
        }
    }
    return(df_scores)
}


get_perc_df <- function(dfs, survey_questions, survey_meta_data) {
    #' -- merges a list of dfs with scores to a big df with percentages -- '#

    all_score_df <- data.frame()

    for (id in names(dfs)) {
        df <- dfs[[id]]
        if (length(df) != 0) {

            score_df <- calculate_scores(df, id, survey_questions, survey_meta_data)

            # if df is empty, overwrite
            if (length(all_score_df) == 0) {
                all_score_df <- score_df
            # if df is not empty, append
            } else {
                all_score_df <- rbind(all_score_df, score_df)
            }
        }
    }

    return(all_score_df)
}

create_backup_table <- function(con) {
    #' -- writes the previous data to a back-up table in case something goes wrong -- '#

    QUERY <- "DROP TABLE IF EXISTS dac_usr.UMBRELLA_SCORES_backup"
    dbExecute(con, QUERY)

    QUERY <- "SELECT * INTO dac_usr.UMBRELLA_SCORES_backup FROM dac_usr.UMBRELLA_SCORES"
    dbExecute(con, QUERY)
}

write_to_db <- function(data) {
    #' -- writes all the data to the database '-- #

    con <- dbConnect(odbc(), Driver = db_driver, Server =db_server,  Database = db_database, UID = db_uid,PWD = db_password, Port = db_port)

    # create_backup_table(con)

    colnames(data) <- c("UMBRELLA_ID", "DOMAIN", "PERIOD", "SCORE")
    data[is.na(data)] <- NA   # writeTable crashes if it is NaN or NULL
    dbWriteTable(con, "UMBRELLA_SCORES", as.data.frame(data), overwrite=TRUE, field.types=c(UMBRELLA_ID="varchar(50)", DOMAIN="varchar(50)", PERIOD="int", SCORE="float"))

    dbDisconnect(con)
}


survey_info <- list(
            "Algemene Kwaliteit van Leven" = c("C30_29_T", "C30_30_T"), 
            "Lichamelijk functioneren" = c("C30_1_T", "C30_2_T", "C30_3_T", "C30_4_T", "C30_5_T"), 
            "Emotioneel functioneren" = c("C30_21_T", "C30_22_T", "C30_23_T", "C30_24_T"), 
            "Sociaal functioneren" = c("C30_26_T", "C30_27_T"),
            "Cognitief functioneren" = c("C30_2O_T", "C30_25_T"), 
            "Vermoeidheid symptomen" = c("C30_10_T", "C30_12_T", "C30_18_T"),
            "Pijn symptomen" = c("C30_9_T", "C30_19_T"),
            "Sexueel functioneren" = c("BR23_44_T", "BR23_45_T"), 
            "Arm symptomen" = c("BR23_47_T", "BR23_48_T", "BR23_49_T"), 
            "Borst symptomen" = c("BR23_50_T", "BR23_51_T", "BR23_52_T", "BR23_53_T")
)

survey_meta_data <-  list(
            "Algemene Kwaliteit van Leven" = list("max_score" = 7, "inverted" = FALSE), 
            "Lichamelijk functioneren" =  list("max_score" = 4, "inverted" = TRUE), 
            "Emotioneel functioneren" = list("max_score" = 4, "inverted" = TRUE), 
            "Sociaal functioneren" =  list("max_score" = 4, "inverted" = TRUE),
            "Cognitief functioneren" =  list("max_score" = 4, "inverted" = TRUE), 
            "Vermoeidheid symptomen" =  list("max_score" = 4, "inverted" = TRUE),
            "Pijn symptomen" =  list("max_score" = 4, "inverted" = TRUE),
            "Sexueel functioneren" =  list("max_score" = 4, "inverted" = FALSE), 
            "Arm symptomen" =  list("max_score" = 4, "inverted" = TRUE),
            "Borst symptomen" = list("max_score" = 4, "inverted" = TRUE)
)

is_time_to_update <- function(last_update_date) {
    # -- checks if it is time to update -- #
    current_date <- Sys.Date()
    current_day <- weekdays(current_date)

    # if it is a sunday and it did not already update on that sunday
    if (current_day == 'Sunday' & last_update_date != current_date) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

update_data <- function() {
    # -- updates the data in the database using the castor API -- #

    print(paste0("Updating data at ", Sys.time()))
    api <- get_api_connection(clientkey, clientsecret)

    umbrella_ids <- get_umbrella_ids(api, study_id)
    print('Got umbrella IDS')

    prefixes <- unname(unlist(survey_info))  # gets all key names from a List
    list_of_dfs <- suppressMessages(get_all_data(api, study_id, umbrella_ids, prefixes))
    print('Got data')

    scores <- get_perc_df(list_of_dfs, survey_info, survey_meta_data)
    print('Calculated percentages')
    
    write_to_db(scores)
    print('Wrote to database')

    print(paste0("Updated data at ", Sys.time()))
}

source("/config.R")
last_update_date <- '2000-01-01'   # initialize date as old date so that when starting script it updates
# updates every week
while (TRUE) {
    if (is_time_to_update(last_update_date)) {
        tryCatch({
            print("start")
            update_data()
            last_update_date <- Sys.Date()
            print("end")
        }, error = function(e) {
            print(e)
            print(paste0("could not update at: ", Sys.time()))
        })
    }
    print("Sleep")
    Sys.sleep(3600)  # checks every hour (cannot increase this as the API will go to sleep or something else happens (bug))
}







