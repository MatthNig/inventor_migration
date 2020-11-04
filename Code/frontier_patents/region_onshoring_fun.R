#################################################
# Description: Function to calculate the number #
#              of onshored high-impact patents  #
#              owned by foreign firms but       #
#              co-invented by domestic residents#
#              for regions and tech_fields in   #
#              the onshoring country            # 
# Authors:     Matthias Niggli/CIEB UniBasel    #
# Date:        04.11.2020                       #
#################################################

region_onshoring_fun <- function(df, onshoring_country = "US"){
        
        onshored_inv_region <- setDT(df)[onshored == 1 & ctry_inv == onshoring_country, 
                                         .(onshored_patents = uniqueN(p_key)),
                                         by = .(regio_inv, p_year)]
        
        onshored_inv_region <- onshored_inv_region %>% group_by(p_year) %>% 
                mutate(total_onshored_patents = sum(onshored_patents),
                       regional_share = onshored_patents / total_onshored_patents)
        
        return(onshored_inv_region)
        }

