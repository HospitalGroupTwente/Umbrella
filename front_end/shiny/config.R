# database where the questionnaire data is saved (obtained via the back-end)
umbrella_db_driver <- ""
umbrella_db_server <-  ""
umbrella_db_database <-  ""
umbrella_db_uid <-  ""
umbrella_db_password <-  ""
umbrella_db_port <- 0

# database from which the UMBRELLA patients can be fetched and the appointments of each doctor
patient_db_driver <- ""
patient_db_server <-  ""
patient_db_database <-  ""
patient_db_uid <-  ""
patient_db_password <-  ""
patient_db_port <- 0

# this is used by the dashboard to obtain the appointments per doctor
# it is inserted into a query which is send to the database
# a bit tedious programming, but some doctors have a filter on ARTS and the other on SUBAGENDA
# Example of ARTS or SUBAGENDA (SA):
# list("NAME1" = c("ARTS", "ARTS CODE"), "NAME2" = c("SA", "SUBAGENDA CODE"), etc..)
name_code_dict <- list("Name1" = c("ARTS", '0000'),  "Name2" = c("ARTS",'0000'), "Name2" = c("SA",'0000'))