### Importa dicionário BFI

    library(readxl)
    dic_bfi <- read_excel("Gabaritos_sertaozinho_2017/dic_bfi_1_2_sertaozinho_2017.xlsx", 
        sheet = "bfi2_dic")

#### Estudando mudança na função

    library(dplyr)
    
    weights <- dic_bfi %>% filter(in_survey ==1) %>%
        group_by(domain, pole) %>%
        count() %>%
        group_by(domain) %>% 
        mutate(fr = sum(n), w = n/fr) 
    
    dic_bfi <- left_join(dic_bfi,  weights, by = c("domain", "pole"))

    test <- apply(data[ ,v_aq], 1, function(x) {mean(x, na.rm=TRUE)} )