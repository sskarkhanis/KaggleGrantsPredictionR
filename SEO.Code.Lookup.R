
# SEO Code
# 

# 61, 'DEFENCE'
# 62, 'PLANT PRODUCTION AND PLANT PRIMARY PRODUCTS'
# 63, 'ANIMAL PRODUCTION AND ANIMAL PRIMARY PRODUCTS'
# 64, 'MINERAL RESOURCES (EXCL. ENERGY)'
# 65, 'ENERGY RESOURCES'
# 66, 'ENERGY SUPPLY'
# 67, 'MANUFACTURING'
# 68, 'CONSTRUCTION'
# 69, 'TRANSPORT'
# 70, 'INFORMATION AND COMMUNICATION SERVICES'
# 71, 'COMMERCIAL SERVICES AND TOURISM'
# 72, 'ECONOMIC FRAMEWORK'
# 73, 'HEALTH'
# 74, 'EDUCATION AND TRAINING'
# 75, 'SOCIAL DEVELOPMENT AND COMMUNITY SERVICES'
# 76, 'ENVIRONMENTAL POLICY FRAMEWORKS AND OTHER ASPECTS'
# 77, 'ENVIRONMENTAL MANAGEMENT'
# 78, 'NON-ORIENTED RESEARCH'
# 99, 'UNKNOWN'
# 00, 'NOT APPLICABLE'

seo.desc <- c('DEFENCE'
               , 'PLANT PRODUCTION AND PLANT PRIMARY PRODUCTS'
               , 'ANIMAL PRODUCTION AND ANIMAL PRIMARY PRODUCTS'
               , 'MINERAL RESOURCES (EXCL. ENERGY)'
               , 'ENERGY RESOURCES'
               , 'ENERGY SUPPLY'
               , 'MANUFACTURING'
               , 'CONSTRUCTION'
               , 'TRANSPORT'
               , 'INFORMATION AND COMMUNICATION SERVICES'
               , 'COMMERCIAL SERVICES AND TOURISM'
               , 'ECONOMIC FRAMEWORK'
               , 'HEALTH'
               , 'EDUCATION AND TRAINING'
               , 'SOCIAL DEVELOPMENT AND COMMUNITY SERVICES'
               , 'ENVIRONMENTAL POLICY FRAMEWORKS AND OTHER ASPECTS'
               , 'ENVIRONMENTAL MANAGEMENT'
               , 'NON-ORIENTED RESEARCH'
               , 'UNKNOWN'
               , 'NOT APPLICABLE'
)

seo.id <-c(61, 
            62, 
            63, 
            64, 
            65, 
            66, 
            67, 
            68, 
            69, 
            70, 
            71, 
            72, 
            73, 
            74, 
            75, 
            76, 
            77, 
            78, 
            00, 
            99
          )

seolookup <- data.frame(cbind(seo.id,seo.desc))
colnames(seolookup) <- c('SEO.Code','SEO.DESC')
