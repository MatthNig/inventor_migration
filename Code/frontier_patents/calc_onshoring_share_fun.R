#################################################
# Description: Function to calculate the share  #
#              of onshored high-impact patents  #
#              owned by foreign firms but       #
#              co-invented by domestic residents#
# Authors:     Matthias Niggli/CIEB UniBasel    #
# Date:        04.11.2020                       #
#################################################

calc_onshoring_share_fun <- function(df){#, onshoring_country){
        
        # Step 1: Calculate the total number of patents by foreign firms and year
        total_pat <- setDT(df)[, .(total_patents = uniqueN(p_key)),
                               by = .(p_year)]

        # Step 2: Calculate the total number of on-shored patents by foreign firms and year
        # onshored_pat <- setDT(df)[ctry_inv == onshoring_country, 
        #                           .(total_patents_onshored = uniqueN(p_key)), 
        #                           by = .(p_year)]
        # => because onshoring-status is now flexibel, this does not work anymore
        
        onshored_pat <- setDT(df)[onshored == 1,
                                  .(total_patents_onshored = uniqueN(p_key)),
                                  by = .(p_year)]

        total_pat <- merge(onshored_pat, total_pat, by = "p_year")
        total_pat$share_onshored <- total_pat$total_patents_onshored / total_pat$total_patents
        
        return(total_pat)
}
