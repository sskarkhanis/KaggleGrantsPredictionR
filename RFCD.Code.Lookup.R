
# 210000 - Science (General)
# 220000 - Social Sciences, Humanities and Arts (General)
# 230000 - Mathematical Sciences
# 240000 - Physical Sciences
# 250000 - Chemical Sciences
# 260000 - Earth Sciences
# 270000 - Biological Sciences
# 280000 - Information, Computing and Communication Sciences
# 290000 - Engineering and Technology
# 300000 - Agricultural, Veterinary and Environmental Sciences
# 310000 - Architecture, Urban Environment and Building
# 320000 - Medical and Health Sciences
# 330000 - Education
# 340000 - Economics
# 350000 - Commerce, Management, Tourism and Services
# 360000 - Policy and Political Science
# 370000 - Studies in Human Society
# 380000 - Behavioural and Cognitive Sciences
# 390000 - Law, Justice and Law Enforcement
# 400000 - Journalism, Librarianship and Curatorial Studies
# 410000 - The Arts
# 420000 - Language and Culture
# 430000 - History and Archaeology
# 440000 - Philosophy and Religion



rfcd.desc <- c('SCIENCE' 
               ,'SOCIAL SCIENCES HUMANITIES AND ARTS'
               ,'MATHEMATICAL SCIENCES'
               ,'PHYSICAL SCIENCES'
               ,'CHEMICAL SCIENCES'
               ,'EARTH SCIENCES'
               ,'BIOLOGICAL SCIENCES'
               ,'INFORMATION COMPUTING AND COMMUNICATION SCIENCES'
               ,'ENGINEERING AND TECHNOLOGY'
               ,'AGRICULTURAL VETERINARY AND ENVIRONMENTAL SCIENCES'
               ,'ARCHITECTURE URBAN ENVIRONMENT AND BUILDING'
               ,'MEDICAL AND HEALTH SCIENCES'
               ,'EDUCATION'
               ,'ECONOMICS'
               ,'COMMERCE MANAGEMENT TOURISM AND SERVICES'
               ,'POLICY AND POLITICAL SCIENCE'
               ,'STUDIES IN HUMAN SOCIETY'
               ,'BEHAVIOURAL AND COGNITIVE SCIENCES'
               ,'LAW JUSTICE AND LAW ENFORCEMENT'
               ,'JOURNALISM LIBRARIANSHIP AND CURATORIAL STUDIES'
               ,'THE ARTS'
               ,'LANGUAGE AND CULTURE'
               ,'HISTORY AND ARCHAEOLOGY'
               ,'PHILOSOPHY AND RELIGION'
               ,'NOT APPLICABLE'
                )

rfcd.id <-c(21,
            22,
            23,
            24,
            25,
            26,
            27,
            28,
            29,
            30,
            31,
            32,
            33,
            34,
            35,
            36,
            37,
            38,
            39,
            40,
            41,
            42,
            43,
            44,
            00
)

rfcdlookup <- data.frame(cbind(rfcd.id,rfcd.desc))
colnames(rfcdlookup) <- c('RFCD.Code','RFCD.DESC')
