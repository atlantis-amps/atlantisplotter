get_groups <- function(groups_csv, thisfolder){

grps <- readr::read_csv(here::here(thisfolder,groups_csv)) %>%
  dplyr::mutate(Code=GroupType) %>%
  dplyr::mutate(GroupType=InvertType)

# set up a functional group types table
vertebrate_groups <- grps %>%
  dplyr::filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>%
  dplyr::mutate(BiomassType="vertebrate")
plankton_groups <- grps %>%
  dplyr::filter(GroupType %in% c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>%
  dplyr::mutate(BiomassType="plankton")
bottom_groups <- grps %>%
  dplyr::filter(GroupType %in% c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','SEAGRASS','PHYTOBEN')) %>%
  dplyr::mutate(BiomassType="2D")
other_groups <- grps %>%
  dplyr::filter(GroupType %in% c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET"))%>%
  dplyr::mutate(BiomassType="other")
biomass_groups <- dplyr::bind_rows(vertebrate_groups,plankton_groups,bottom_groups,other_groups)

# add to grps df
grps <- grps %>%
  dplyr::left_join(biomass_groups) %>%
  dplyr::filter(IsTurnedOn==1)
}
