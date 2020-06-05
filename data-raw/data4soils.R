pacman::p_load(reshape, tidyverse, readxl, purrr, DataPushR, fs)

# DTA FP Sally 10x10 m cells data
cells1 <- read_xlsx(path ="data_updated/Sample Log DTA July 2013 FP Sally copy.xlsx",
                    sheet = "DU1 10-m X10-m Cells", col_names = TRUE, skip = 1)[,1:23]

colnames(cells1) <- c("Name","Date","DU","Type","Dimension","Cellnumber","Replicate","Sampler","Bagger","Increments","IncrementSpacing.m","DateGround","Grinder","Name.remove","MassGround.g","Notes","HPLC","DataAnalysis","Name.remove","DNT24","NG","Notes2","Confirmed")


cells1 <- cells1 %>%
  select(-contains("remove")) %>%
  mutate(DNT24 = signif(DNT24, 2), NG = signif(NG, 2),
         sname = Name %>% map(~str_split(.x, pattern = " Rep ")) %>% map(1) %>% map_chr(1))

# didn't filter anything
#cells1 <- cells1[apply(cells1,1,function(x) sum(is.na(x))<=15),]

fp_sally_dta <- cells1 %>%
  group_by(Cellnumber) %>%
  summarize(DNT24 = mean(DNT24) %>% signif(2),
            NG = mean(NG) %>% signif(2),
            MassGround.g = mean(MassGround.g) %>% signif(2)) %>%
  rename(Cell = Cellnumber) %>%
  mutate(type = "cells",
         row = format(Cell - 1, zero.print = T) %>%
           str_sub(1, 1) %>%
           str_replace(" ", "0") %>%
           as.numeric() %>% `+`(1),
         column = format(Cell, zero.print = T) %>%
           str_sub(3, 3) %>%
           str_replace("0", "10") %>%
           as.numeric())

######  Code to Use Alan and Tom Data  #####
####  Allan and Tom Data from Presentation in 2006 ####
##### Site Characterization:  The Science and What it Means to Operations
####  Found in Crumbling MIS email 6/23/2009

#rdx.mat are the discrete measurements done that Alan used to compare against an MIS
# over the full range.

### datasets that are created.
#
# e7format and e9format are lists with 3 different constituents measured at the
# same locations.  e5 was not formated but could be used.  I am not sure why it has
# two measurements at each location.  Also most are below detection.

###  VOC row column example

# rows.depths, cols.layers are the MIS and discrete.cells are the discrete measurements
# actually taken.

rdx.mat <- matrix(c(17.1,  1.27,0.829,0.908,10.9,4.44,0.437,0.354,1.52,0.067,
                    0.805,24.1,7.73,0.539,0.260,0.233,0.366,1.93,0.731,0.138,
                    30.8,1.40,12.5,0.342,0.074,1.11,0.18,0.076,7.11,0.187,
                    12.7,138,53.7,3.85,4.94,1.22,4.63,0.470,2.41,1.06,
                    331,9.70,3.96,1.44,3.67,0.243,3.21,0.254,1.03,0.073,
                    7.52,5.65,1.97,0.571,4.84,19.9,0.825,0.122,1.46,0.070,
                    1.65,1.56,8.51,10.6,2.24,25.2,7.15,0.248,0.175,0.037,
                    48.3,13.3,3.36,6.93,889,21.8,3.75,0.618,0.193,0.081,
                    1.18,1.03,64.3,557,1790,2390,11.3,1.65,0.335,0.263,
                    8.86,3.50,5.02,42.7,385,24.9,3.64,0.96,0.526,0.161) ,
                  byrow = T, nrow = 10)

# Table E5. Concentration (mg/kg) of energetic residues in discrete samples collected
# at Fort Richardson, Low-order #3. (Page 1 of 6)

#  Table E7 (mg/kg) of energetic residues in discrete samples collected at CFB Petawawa, Hand Grenade Range.
#* Moving NW from SE corner, then SW for each 2 x 2 m sub grid;
#- discrete sample collected in westernmost corner of each 2 x 2 m subgrid"

e7e5 <-read_csv("AllanData/TableE7E5.csv",na = "ERL", col_names = TRUE)

e7 <- e7e5 %>%
  filter(Table == "E7")

e5 <- e7e5 %>%
  filter(Table == "E5")

# "Table E9. Concentration of energetic residues in discrete samples collected at CFB Petawawa, Firing point Juliet Tower."

e9 <-read_csv("AllanData/TableE9.csv", na = "ERL", col_names = TRUE)

### Put e5, e7, and e9 in matrix format.

# old function that still works.  Keeping
ltomat <- function(dataset,measuredcolumn,byrow=T,rows=10){
  dataset <- as.data.frame(dataset)
  #print(colnames(dataset))
  tempi <- vector("list", length(measuredcolumn))
  names(tempi) <- measuredcolumn
  for (i in 1:length(measuredcolumn))
  {
    tempi[[measuredcolumn[i]]] <- matrix(dataset[, measuredcolumn[i]],
                                        nrow = rows, byrow = byrow)
  }
  tempi
}

e5format <- ltomat(e5, c("HMX", "RDX", "TNT"))
e7format <- ltomat(e7,c("HMX","RDX","TNT"))
e9format <- ltomat(e9,c("2,4DNT","NG"))

### VOC Data  ###
# "VOC Data that uses row column example see pdf in VOC folder"

rc.voc <- read_csv("AllanData/VOC_ColumnRow/RowColumnVOCfromAllanPaper.csv",
                   col_names = TRUE, na = c("NR","ns"))

rows.depths <- rc.voc %>% filter(X1 != "mis") %>% pull(mis)
cols.layers <- filter(rc.voc, X1 == "mis") %>% select(-1, -mis) %>% unlist() %>% as.numeric()

## I decided that nd or non-detects got zero  ##
##  NA are not measured  #####
discrete.cells <- rc.voc %>%
  filter(X1 != "mis") %>%
  select(-mis) %>%
  mutate_all(function(x) { ifelse(x == "nd", 0, x)})


names(rows.depths) <- pull(discrete.cells, X1)
names(cols.layers) <- colnames(discrete.cells)[-1]


discrete.cells <- discrete.cells %>%
  select(-X1) %>%
  rename_all(function(x) { str_remove(x, "SB-")}) %>%
  mutate_all(as.double) %>%
  mutate(row = 1:n())

### Now format and name for package.

tce_chuck <- discrete.cells %>%
  pivot_longer(-row, names_to = "column", values_to = "tce") %>%
  rename_all(str_to_lower)

##### Not using E5
# Table E5. Concentration (mg/kg) of energetic residues in discrete samples collected
# at Fort Richardson, Low-order #3. (Page 1 of 6)

#  Table E7 (mg/kg) of energetic residues in discrete samples collected at CFB Petawawa, Hand Grenade Range.
#* Moving NW from SE corner, then SW for each 2 x 2 m sub grid;
#- discrete sample collected in westernmost corner of each 2 x 2 m subgrid"
hmx_data_cfbp_handgrenade <- e7format$HMX %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  pivot_longer(-row, names_to = "column", values_to = "hmx") %>%
  mutate(column = column %>% str_remove("V") %>% as.integer())

rdx_data_cfbp_handgrenade <- e7format$RDX %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  pivot_longer(-row, names_to = "column", values_to = "rdx") %>%
  mutate(column = column %>% str_remove("V") %>% as.integer())

cfbp_handgrenade = left_join(hmx_data_cfbp_handgrenade, rdx_data_cfbp_handgrenade) %>%
  rename_all(str_to_lower)


# "Table E9. Concentration of energetic residues in discrete samples collected at CFB Petawawa, Firing point Juliet Tower."

dnt24_data_cfbp_fpfuliet <- e9format$`2,4DNT` %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  pivot_longer(-row, names_to = "column", values_to = "dnt24") %>%
  mutate(column = column %>% str_remove("V") %>% as.integer())

NG_data_cfbp_fpjuliet <- e9format$NG %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  pivot_longer(-row, names_to = "column", values_to = "NG") %>%
  mutate(column = column %>% str_remove("V") %>% as.integer())

cfbp_fpjuliet <- left_join(dnt24_data_cfbp_fpfuliet, NG_data_cfbp_fpjuliet) %>%
  rename_all(str_to_lower)


rdx_alan <- rdx.mat %>%
  as_tibble() %>%
  mutate(row = 1:n()) %>%
  pivot_longer(-row, names_to = "column", values_to = "rdx") %>%
  mutate(column = column %>% str_remove("V") %>% as.integer()) %>%
  rename_all(str_to_lower)


dta_fpsally <- fp_sally_dta %>%
  select(row, column, DNT24, NG) %>%
  rename_all(str_to_lower)



################# Now Build data package ##############

package_name_text <- "data4soils"
base_folder <- "../../byuidatascience/"
user <- "byuidatascience"
package_path <- str_c(base_folder, package_name_text)

####  Run to create repo locally and on GitHub.  ######

# github_info <- dpr_create_github(user, package_name_text)
#
# package_path <- dpr_create_package(list_data = NULL,
#                                      package_name = package_name_text,
#                                      export_folder = base_folder,
#                                      git_remote = github_info$clone_url)

##### dpr_delete_github(user, package_name_text) ####

####### End create section
github_info <- dpr_info_github(user, package_name_text)
usethis::proj_set(package_path)

usethis::use_data(tce_chuck, rdx_alan, cfbp_fpjuliet, cfbp_handgrenade, dta_fpsally)

dpr_export(tce_chuck, export_folder = path(package_path, "data-raw"),
           export_format = c(".csv", ".json", ".xlsx", ".sav", ".dta"))

dpr_export(rdx_alan, export_folder = path(package_path, "data-raw"),
           export_format = c(".csv", ".json", ".xlsx", ".sav", ".dta"))

dpr_export(cfbp_fpjuliet, export_folder = path(package_path, "data-raw"),
           export_format = c(".csv", ".json", ".xlsx", ".sav", ".dta"))

dpr_export(cfbp_handgrenade, export_folder = path(package_path, "data-raw"),
           export_format = c(".csv", ".json", ".xlsx", ".sav", ".dta"))

dpr_export(dta_fpsally, export_folder = path(package_path, "data-raw"),
           export_format = c(".csv", ".json", ".xlsx", ".sav", ".dta"))


variable_names = list(
  row = "The row where the sample was collected",
  column = "The column where the sample was collected",
  dnt24 = "The amount of 2,4 DNT in mg / kg",
  ng = "The amount of NG in mg / kg",
  rdx = "The amount of RDX in mg/kg",
  tce = "The amount of TCE in mg/kg",
  hmx = "The amount of HMX in mg/kg"
)

usethis::use_data(tce_chuck, rdx_alan, cfbp_fpjuliet, cfbp_handgrenade, dta_fpsally)

dpr_write_script(folder_dir = package_path, r_read = "Phase II Final Report Draft_Final_submitted.pdf",
                 r_folder_write = "data-raw", r_write = "dta_fpsally.pdf")

dpr_document(rdx_alan, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "rdx_alan",
             title = "Data obtained from an Email from Alan Hewitt",
             description = "Discrete samples on a grid to compare to DU wide MIS",
             source = "Email",
             var_details = variable_names)



dpr_document(cfbp_fpjuliet, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "cfbp_fpjuliet",
             title = "Data from multi-increment sampling at Ft. Richardson",
             description = "Validation of Sampling Protocol and the Promulgation of Method Modifications for the Characterization of Energetic Residues on Military Testing and Training Ranges",
             source = "https://serdp-estcp.org/content/download/5167/73264/file/ER-0628-FR.pdf",
             var_details = variable_names)



dpr_document(cfbp_handgrenade, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "cfbp_handgrenade",
             title = "Data from multi-increment sampling at Ft. Richardson",
             description = "Validation of Sampling Protocol and the Promulgation of Method Modifications for the Characterization of Energetic Residues on Military Testing and Training Ranges",
             source = "https://serdp-estcp.org/content/download/5167/73264/file/ER-0628-FR.pdf",
             var_details = variable_names)



dpr_document(dta_fpsally, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "dta_fpsally",
             title = "Phase 2 Final Report: Incremental Sampling Visual Sample Plan Validation Study at Firing-Point Sally",
             description = "The objective of this project is to develop and demonstrate cost-effective statistical sampling techniques that can be used to confidently estimate the magnitude and extent of energetic compounds (i.e., explosives and propellants) within wide-area DUs when using ISMs.",
             source = "data-raw/dta_fpsally.pdf",
             var_details = variable_names)



dpr_document(tce_chuck, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "tce_chuck",
             title = "MULTI INCREMENT TCE Vadose-Zone Investigation",
             description = "MULTI INCREMENT and discrete sampling strategies were used to estimate the average concentration and the three‐dimensional distribution of TCE in a 3,300‐m3 zone composed of two decision units.",
             source = "https://onlinelibrary.wiley.com/doi/pdf/10.1002/rem.20196",
             var_details = variable_names)


dpr_readme(usethis::proj_get(), package_name_text, user)

dpr_write_script(folder_dir = package_path, r_read = "scripts_updated/explosives_data_data4.R",
                 r_folder_write = "data-raw", r_write = str_c(package_name_text, ".R"))

devtools::document(package_path)

dpr_push(folder_dir = package_path, message = "'first data push'", repo_url = NULL)


