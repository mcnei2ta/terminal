# Clear environment
rm(list = ls())
# Clear console
cat("\014")

library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(plotly)
library(openxlsx)
library(scales)

### If having trouble installing packages:
# execute this in console: trace(utils:::unpackPkgZip, edit=TRUE)
# find Sys.sleep(0.5) --> change to Sys.sleep(2.5)

################################################ UPDATES

# set working directory
setwd("/Users/thomasmcneill/Documents/data/spendingdocs")


# create a list of the names of all files in the working directory, call the list "files"
files <- paste(substr(list.files(), 1, nchar(list.files())))

# read in all the files in the working directory, name them according to their filenames
for(i in 1:length(files)){
  assign(files[i],fread(files[i], check.names = TRUE))
}

# Combine checking account files
checking = data.frame()

for(i in ls(pattern = 'Acct_5789*')){
  temp <- get(i) %>% select(Date, Description, Amount)
  checking <- rbind(checking, temp)
}

checking$Payment_Method <- 'Debit'

# change amount to numeric
checking$Amount <- gsub("\\(", "-",checking$Amount)
checking$Amount <- gsub("\\)", "",checking$Amount)
checking$Amount <- gsub("\\$", "",checking$Amount)
checking$Amount <- gsub(",", "",checking$Amount)
checking$Amount <- as.numeric(checking$Amount)


# combine credit card account files
credit = data.frame()

for(i in ls(pattern = 'Acct_7493*')){
  temp <- get(i) %>% select(Date, Description, Amount)
  credit <- rbind(credit, temp)
}

rm(temp)

# add negative signs to positive transactions
credit$Amount <- ifelse(grepl("\\(",credit$Amount),
                        credit$Amount,
                        paste0("-",credit$Amount))

credit$Payment_Method <- 'Credit'

# change amount to numeric
credit$Amount <- gsub("\\(", "",credit$Amount)
credit$Amount <- gsub("\\)", "",credit$Amount)
credit$Amount <- gsub("\\$", "",credit$Amount)
credit$Amount <- gsub(",", "",credit$Amount)
credit$Amount <- as.numeric(credit$Amount)

# Combine credit and checking transactions
spending <- rbind(credit, checking)

# Remove items from environment
rm(list=ls(pattern = 'Acct*'))
rm(credit)
rm(checking)

#### Earliest date to be included
Earliest <- "2020-09-01"
Latest <- "2021-09-01"

##################################################################################################################


purchases = list(
  credit_payments = c("M-APP TRANSFER US","PAYMENT TO CREDIT CARD","CR CD PMT"),
  gym = c('GOLDS GYM'),
  stores_groceries = c('CVS/PHARMACY', 'RITE AID STORE', "UPPY'S", 'WAWA','CHEVRON/QUICKWAY MART','HUDSON NEWS','TOM THUMB STORE','TOMMYS 27',
                       'TROLLEY MARKET','QT 885 0800', 'SHELL SERVICE STATION','BP#9708157CARY ST','Z MARKET','UNIVERSAL WINE & L', 'HUDSON',
                       'VA ABC STORE','UNION WINE AND LIQ','7-ELEVEN','COLUMBIA PIKE CI', 'FAMILY DOLLAR','WALGREENS','SHEETZ','REAMS MARKET',
                       "AKJK CROWN",'BI-LO GROCERY','SPEEDWAY',"SPEEDY'S MART",'SUNNYS MARKET','CHEVRON FOOD MART','PUBLIX SUPER MAR',
                       'SAFEWAY','NNT QUICK SNACK','HARRIS TEETER', 'KROGER','FOOD LION','CAP LIQUORS','SHELL OIL','TOKYO MARKET','TARGET',
                       "MARATHON PETRO","QT 770 CONYERS","3 HENS","MECKLENBURG ABC","STRAWBERRY ST MARKET","BP RICHMOND","PUBLIX",
                       "MILE HIGH LIQUORS","CST4076 DENVER","TOM LEONARD'S FARMERS MA","ALDI","FRESH MARKET"
  ),
  household_items = c("LOWE'S",'BEDBATH&BEYOND','IKEA','NNT COST PLUS', 'GOODWILL','BED BATH & BEYOND','STAPLES',"LOWES",
                      "U-HAUL","ADDRESSCHANGEFORMS","RICHMOND RE-CYC"
  ),
  bars = c('CRYSTAL CITY SPORT', 'EL REY', 'FOX BROS BAR','LUCKY BAR',"ORMSBY'S",'SINE IRISH PUB','SOUTHERN RAILWAY T','ROCK AND ROLL',
           'THE QUEEN VIC','THE FAINTING GOAT','ZEN BISTRO','BAJA BEAN','BARCODE','BASEMENT BAR','DON`T LOOK BACK','Flash','FLORA',
           'HUDSON ST1468','LUCKY STRIKE WASHI','Mission','PEARL RAW BAR','POSTBELLUM','SARAHS PLACE','Sticky Rice','WHISKEY BUSINESS',
           'YEOLDEBULLANDBUSH','LOCAL 16','Wet Dog Tavern','THE BRIGHTON','DC SANTA CRAWL','PIK NIK','Mockingbird Hill','CITY TAP HOUSE',
           'ABIGAIL','CAFE CITRON','JEFFERSON TJS REST','F.W. SULLIVANS','WHITLOWS','CITY TAP','MARRIOTT RICHMOND','THUNDER GRILL',
           'THE BROADBERRY','ALL SOULS','El Centro','BAR DECO','Rocket Bar','BAR LOUIE','FLASH','TIKI TNT','PUNCH BOWL',
           "OSULLIVANS IRISH P", "COURTYARD RICHMOND", "WILSON HARDWARE", "DC 9", "AMERICAN ICE COMPA", "CRAFTHOUSE",'OLD SMOKEY BAR',
           'NIGHTCLUB','TILLYS TIKI BAR','THE BEER LOT', 'EIGHTEENTH STREET','CAPITOL LOUNG','THE ANTHEM','GOAT','SOCIAL', 'QUINNS',
           ' SAUF HAUS','MOES','BULLPEN','SAUF HAUS','KITH AND KIN','H STREET COUNTRY C','Bluejacket/The Yar', 'REBELLION ON',
           "THE LIBERTY T", "U STREET MUSIC HAL","STICKY RICE","POOR BOYS","Roofers Union",'801 Restaurant and Bar','CARY STREET CAFE',
           "GARDEN GROVE BREWING","BARRIO","BUSHWALLER","IDIOM BREWING CO","THE HOP AND VINE","STEINHARDT BREWING CO","THE WINE KITCHEN",
           "White Rabbit Gastropub","CAFE NOLA","CANON & DRAW BREWING","BLUE MOON BREWING","GREAT DIVIDE BREW","BLACK SHIRT BREWING",
           "CANOPY DENVER","CANON & DRAW"
  ),
  eating_out = c("BEAUVINE BURGER", "DESPERADOS BURGER", "BASIC BURGER", "BURGER KING", "BURGER BACH",'SAVI PHARR', 'STARBUCKS',
                 'AUNTIE ANNES', 'BREZZA CUCINA','BUCKHEAD IRISH PUB','CHIPOTLE','KOGIYA RESTAURANT',
                 "MCDONALD'S", "PAPA JOHN'S", 'RICHMOND AIRPORT','DC DONER',
                 'NASSTA CORP','PARS KABOB',"TGI FRIDAY'S",'AN UNCOMMON CAFE','BESTOLLI PIZZA','BONEFISH GRILL','Dominos',
                 'CARLYLE','CHICK-FIL-A','JAMBA JUICE','JOHNNY PISTOLAS', 'DOSI ROCK','MARY ANGELAS PIZZE','MUKUNDRAI INC',
                 'NY DELI','PANERA BREAD','POPEYES',"ALEXA'S FRIED","ALEXA'S FRIED",'IRON GRILL','SWEETBAKE',
                 'DCTAPASTRU','SUGAR & TWINE','LOCAL EATERY','WOODSHED SMOKE',"WENDY'S",'JIMMY JOHNS','KIMBERLY SAVIL',
                 'SHAKE SHACK','SIDEWALK CAFE','CHANELLOS PIZZA','LAMPLIGHTER','SUBWAY','JEFFERSON LEMAIRE','WENDYS','LEVELUP*CAVA',
                 'CAFE TWELVE','SMALLCAKES CUPCAKE','UPPER SHIRLEY VINE',"KELLY'S CAJUN",'SAISON','TACO BELL','TEMPLE','KFC',
                 'CRACKER BARREL','SMOOTHIE KING','TNR CAFE','TARRANTS','TOPPERS PIZZA','CITY DOGS','CAVA','COOK OUT','CHICKFILA',
                 'VCUMEDICALAUBONPAI',"IRONCLAD COFFE", "PEETS", "KOBE JAPANESE STEA","RICHARDS RESTAURAN", "OLD EBBITT GRILL", 
                 "THE SIMILANS THAI", "ARBYS", "MCDONALDS","FETG2", 'HOME TEAM GRILL','CAFE GEORGETOWN','MARTINS TAVERN',
                 "SWIZZLER FOOD", '2WSHGTN NTNLS','BREAD & WATER', 'CREATIVE FOOD', 'OLD CHICAGO COLUMB', 'RED LOBSTER','BBQ',
                 'BAR TACO','J CHRISTOPHERS BUC', 'CHICK FIL A', 'SBARRO', "NATE'S BAGELS", 'OEGADGIB','NAMASTE EVERE','NEW YORK DELI',
                 'EN SU BOCA','BONEFISH', 'RESTAURANT', 'RHODESIDE GRILL', "IRELAND'S FOUR COU", 'COMPASS COFFEE','BOB & EDITHS DINER',
                 'KUBA KUBA', 'THE ANNEX', 'NANDO S PERI-PERI','LEMON CUISINE','HERITAGE','URBAN THAI RESTAUR','ANOTHER BROKEN EGG',
                 'WHITEHALL TAV','IBERIAN PIG','AMORINO ATLANT','LITTLE MEXICO', 'EPIC SMOKEHOUSE', "KIRWAN'S ON THE WH",
                 'DOLCEZZA GELATO', 'NEW YORK STYLE','SUSHI HIBACHI BIST','BANGKOK 54 RESTAUR', 'FUEL PUMP', 'WAFFLE HOUSE','A DELI',
                 'PIZZA & BEERS', '&PIZZA',"JACK BROWNS","THE TOBACCO COMPAN","THE FRANKLIN INN","DOMINO'S","BOULEVARD BURGER",
                 "SMOOTHIE","ROAEATSTR",'FIRE & HOPS','IHOP','OYAMEL',"ROSTOV'S COFFEE & TEA",'KPHTH LLC','FOODA INC','GREEK ON CARY',
                 'POTBELLY','PANDA EXPRESS','DC FOOD TRUCK',"CAPITAL CAFE",'TASTY KABOB',"FLIPPN' PIZZA","TASTYKABOB","PETER CHANG",
                 "BACCHUS","NATES BAGELS","FU JIAN CHINESE","JAVA SURF CAFE","GRUBHUB.COM","ASSANTES PIZZA","CAFE ANGLAIS",
                 "BEANS & BAGELS LEWISTOWN","DD/BR","ROASTOLOGY CAFE","Four Brothers Grill","CRAZY THAI","LITTLE SPOON","PAPA JOHNS",
                 "BELMONT PIZZERIA","SHYNDIGZ","AC DBC FS PIZZA TENNYSON","El Jefe Denver","FRESCA Denver","STYRIA BAKERY II",
                 "TNT COUNTRY KITCHEN","SASSAFRAS","THE BLUE COW EATERY","BOMBOLINI PASTA","COFFEE","QUICK SNACK","ANKARA",
                 "IZZY S KITCHEN","TARRANT S","TEXAS DE BRAZIL","GARNETT`S","OK8","BOKA TAKO","AMK CAP1","TOAST",
                 "BAMBOO CAFE", "CURBSIDE","DUNKIN","MELLOW MUSHROOM","CARYTOWN BURGERS","THE BOATHOUSE","PIT AND THE PEEL",
                 "VILLAGE CAFÉ","VILLAGE CAFE"
                 ),
  #  lunch = c ('POTBELLY','PANDA EXPRESS','DC FOOD TRUCK',"CAPITAL CAFE",'TASTY KABOB',"FLIPPN' PIZZA","TASTYKABOB","PETER CHANG"),
  clothes_shoes = c('GAP US','NORDSTROM','MACYS','T.J. MAXX','JOSABANK CLOTHIERS',"JOSABANK",
                    'DSW PENTAGON ROW',"FINISHING TOUCHES","DSW","OLD NAVY"
  ),
  electronics_movies_books = c('BEST BUY','WAL-MART','NINTENDO','GOOGLE','BARNESNOBLE','HBOOKSELLER','M JUDSON BOOKSELLE',"INSTYLE FIX",
                               "Kindle","APPLE STORE"),
  #  insurance = c('STATE FARM INSURAN','STATE FARM INSURA'),
  fun_activities = c('AGECROFT HALL','DEGGELLER ATTRACTI','STATE FAI','MOVIELAND','REGAL POTOMAC YARD','RUSSELL FOODS',
                     'STONE MOUNTAIN','MOONRISE FESTI','STOCKYARD SPORTS','WM SUPERcenter','DC BRUNCH','REGAL CINEMAS',
                     'POE FOUNDATION','WINTERGREEN','DAYS INN','RESIDENT ADVISOR T',"PARTY CITY",'TICKETFLY','TRIGGER AGENCY',
                     'STUBHUB','FCPA OAK MAR GC','INTL SPY MUSEUM','TICKETMASTER','BALLSTON COMMON ST','CARMACK',"VIRGINIA MUSEUM BEST", 
                     "ARTECHOUSE","LINCOLN THEATRE",'ALOHA SAFARI ZOO',"FIRST LANDING ST PARK","NATIONAL MUSEUM OF CIVIL FREDERICK",
                     "BOGEYS SPORTS PARK","DOG KRAZY","VIRGINIA MUSEUM OF FINE","WHITE TAIL","WHITETAIL"
                     ),
  gifts_holidays = c('PREPAID','MOOSE APPLE CHRIST','Amzn.com/bill','ACACIA MID-TOWN','GAMESTOP #4101 1100 S HAY 12-23-18',
                     'VZW WEBPAY VZ WIRELESS','BRANDYLANE PUB',"L2ATOM51",'RICHARD VARIETY ST','GUAVA FAMILY INC',
                     'GAMESTOP #4101 1100 S HAY 12-03- 19','THINGS REMEMBERE','5S5RZ6S','TARGET.COM',"MICHAELS STORES",
                     "RAVENCHASE", "MONGREL","COURANT","LADLESLINE","PHANPHNGSD","PICTURESTH"
                     ),
  #  groceries = c('HARRIS TEETER', 'KROGER','FOOD LION'),
  media = c('ITUNES','Hulu','Prime Video','Spotify', 'Amazon Prime','APPLE.COM/BILL',"SPOTIFY"),
  phone = c('PAYMENTS VERIZON WIRELESS','VERIZON WRLS'),
  internet = c('VERIZON .COM','VERI ZON FL'),
  rent_utilities = c('PROPERTY PAYMENT','STATE FARM INSURAN','STATE FARM INSURA',"AMPLE STORAGE",
                     "PAYMENT VCU WEB PAY 508978","CHECK"),
  salary_and_reimbursement = c('DIRECT DEP BOOZ ALLEN HAMIL','MOBILE DEPOSIT',"FUNDS XFER BRENTALIE", "VASTTAXRFD VA DEPT TAXATION", 
                               "SBTPG LLC TAX PRODUCTS",'TAX REF IRS',"TRUIST CARES BONUS","AUTO PMT FROM ACCT","IRS TREAS"),
  student_loans_grad_school = c('STUDNTLOAN',"COLLEGE TRANSCRIPT","VCUGRAD"),
  transfer_from_savings = c('FROM SAVINGS'),
  transfer_to_savings = c('TO SAVINGS'),
  investments = c("COINBASE"),
  metro = c('ERM','METRO'),
  trains_buses_car_repair = c('AMTRAK','GREYHOUND','MEGABUS',"RVA COMPLETE AUTOMOTIVE","AUTO REPAIR"),
  travel = c('SPIRIT AIRL','DELTA AIR','MARC GARAGE & BWI','PEN AND PROSE - BWI','UNITED','EXPEDIA','QUALITY SUITES',
             'DULLES WASHINGTON','HAMPTON INN','FOREIGN TRANSACTION FEE','CONDOR',"ENTERPRISE RENT-A-CAR"),
  rideshares = c('UBER','LYFT','BIRD APP'),
  venmo_paid = c('CASHOUT VENMO'),
  venmo_payment = c('PAYMENT VENMO','RETRY PYMT VENMO'),
  miscellaneous = c('ATM',"LEONARD'S STUD","PAULINEOGEM VISA MONEY TRANSFER",
                    'CMSVEND','EAST POTOMAC GOLF','PENTAGON center TR','BLCKT VE','USCONNECT ACCNT VE','USPS.COM','HUMAN RIGHTS',
                    'GREAT CLIPS', 'COINMACH RIVER','PARKMOBILE','RIVER HOUSE VALET','VALET PARKIN',"RETURNED ITEM FEE",
                    'PENSKE TRK', 'PAY PAL *PAULINEOGEM', 'FANTASTIC THRIF','TEN THOUSAND', 'VNO PENTAGON PLAZA','COINMACH',
                    "L'ENFANT WEST","MYEYEDR","ARAMARK REFRESHMENTS","FMB LAUNDRY","INTUIT",'EXXONMOBIL',
                    "IMMORTAL BELOVED","ASHLEY-RIVERHOUSE","TELADOC","Great Clips","WIKIPEDIA","UPS STORE"
  ),
  online_purchases = c('AMZN DIGITAL','Amazon.com*MB7H24C','AMZN Mktp','JIBJAB ECARDS',"AMAZON.COM","AMZN MKTP",
                       "DATACAMP INC","AMZN Digital")
)



# Miscellaneous: Laundry, Haircut, Parking, Books, Withdrawal, Snack, Shipping Online Purchase, Donations, Scam

### create new column Purchase
spending$Purchase = ""

for(key in purchases){
  for(value in key){
    spending$Purchase <- ifelse(grepl(value, spending$Description), value, spending$Purchase)
  }
}

## Create list of nulls for adding values as new purchases arise
# inspect null dataframe and include new keywords
spending_null <- spending %>% filter(spending$Purchase == "")

# If there are any null purchases, stop running and categorize the new purchases
if(nrow(spending_null) > 0)
  stop('New purchases need to be categorized')

rm(spending_null)

### create new column Category
spending$Category = ""

for(i in 1:NROW(purchases)){
  for(value in purchases[i]){
    for(n in value){
      spending$Category <- ifelse(spending$Purchase %in% n, names(purchases)[i], spending$Category)
    }
  }
}

rm(purchases)

### Filter out credit payments
spending <- spending%>%
  filter(Category!="credit_payments")

##### Filter out / edit Miscellanious stuff (trip to Romania cancelled, tickets returned)
spending <- spending %>%
  filter(Purchase!="CONDOR")


### create new column Type
types = list(
  day_to_day = c('eating_out','stores_groceries','rideshares','bars', #'lunch', #groceries',
                 'metro','extra','trains_buses_car_repair','laundry','snack','withdrawal','haircut','books','shipping_online_purchase',
                 'parking','fun_activities','miscellaneous'),
  monthly_bills = c('media','phone',#'insurance','gym',
                    'student_loans_grad_school','rent_utilities','internet'),
  ad_hoc_purchases = c('clothes_shoes','travel','electronics_movies_books',
                       'gifts_holidays','household_items','donations','online_purchases'),
  income = c('salary_and_reimbursement'),
  savings_investments = c('transfer_from_savings','transfer_to_savings','investments'),
  venmo = c('venmo_payment','venmo_paid')
)

spending$Type = ""

for(i in 1:NROW(types)){
  for(value in types[i]){
    for(n in value){
      spending$Type <- ifelse(spending$Category %in% n, names(types)[i], spending$Type)
    }
  }
}

rm(types)

### Create new column City
city = list(
  c("ALEXANDRIA",   "ANNANDALE",    "ARLINGTON",    "ATLANTA",    "BALTIMORE",    "BENBROOK",    "BERRYVILLE",    "BOWIE",    "CENTREVI LLE",
    "CHANTILLY",    "CHARLESCITY",    "DFWAIRPORT",    "EASTRUTHERFO",    "FAIRFAX",  "FALLSCHURCH",    "FAYETTEVILLE",    "FORESTHEIGHT",
    "EMPORIA", "FT.WORTH",    "FTWORTH",    "GLENALLEN",    "GREENVILLE",    "HALETHORPE",   "HENRICO",  "HERNDON", "IRVING",  "LADYSMITH",
    "LAVONIA",    "LINTHICUMHEI",    "NEWARK",    "OAKTON",    "PARAMUS",    "RICHMOND",    "ROSSLYN",    "RUTHERGLEN",    "SANFRANCISCO",
    "SMITHFI ELD",    "SPRING L AKE",    "WARRENTON",    "WASHIGNTON",    "WASHINGTON",    "WAYNESBORO",    "WINTERGREEN", "FREDERICK"
  )
)

spending$City = ""

for(key in city){
  for(value in key){
    spending$City <- ifelse(grepl(value, toupper(gsub(' ','', spending$Description))), value, spending$City)
  }
}

rm(city)

### Create new column State
state = list(
  #c(" DC ", " VA ", " MD ", " CA ", " NJ ", " GA "," TX "," DE "," WA ")
  " AK "," AZ "," AR "," CA "," CO "," CT "," DE "," FL "," GA "," HI "," ID "," IL "," IN "," IA "," KS "," KY "," LA "," ME "," MD ",
  " MA "," MI "," MN "," MS "," MO "," MT "," NE "," NV "," NH "," NJ "," NM "," NY "," NC "," ND "," OH "," OK "," OR "," PA "," RI ",
  " SC "," SD "," TN "," TX "," UT "," VT "," VA "," WA "," WV "," WI "," WY "," DC ",
  " A K "," A Z "," A R "," C A "," C O "," C T "," D E "," F L "," G A "," H I "," I D "," I L "," I N "," I A "," K S "," K Y "," L A ",
  " M E "," M D "," M A "," M I "," M N "," M S "," M O "," M T "," N E "," N V "," N H "," N J "," N M "," N Y "," N C "," N D "," O H ",
  " O K "," O R "," P A "," R I "," S C "," S D "," T N "," T X "," U T "," V T "," V A "," W A "," W V "," W I "," W Y "," D C "
)

spending$State = ""

for(key in state){
  for(value in key){
    spending$State <- ifelse(grepl(value, spending$Description), value, spending$State)
  }
}
spending$State <- gsub(' ','', spending$State)

rm(state)

## format Purchase, Category and Type
spending$Purchase <- toupper(gsub('_', ' ', spending$Purchase))
spending$Category <- toupper(gsub('_', ' ', spending$Category))
spending$Type <- toupper(gsub('_', ' ', spending$Type))
spending$City <- toupper(spending$City)

spending$Type <- factor(spending$Type, levels = c("INCOME",
                                                  "MONTHLY BILLS",
                                                  "DAY TO DAY",
                                                  "VENMO",
                                                  "AD HOC PURCHASES",
                                                  "SAVINGS INVESTMENTS"))

### DATE

# Create a function that gets the date from the Description (Date variable is wrong)
# Date formats: 00-00-00 and 00-00 (MM/DD(/YY))

# Create Short_Date
for(j in 1:NROW(spending)){
  spending$Short_Date[j] = ""
  for(i in 1:nchar(spending$Description[j])){
    chunk = substr(spending$Description[j], i, i+6)
    if(substr(chunk, 1, 1) == " " & substr(chunk, 4, 4) == "-" & substr(chunk, 7,7) == " "){
      spending$Short_Date[j] <- chunk
    }
  }
}

#Create Long_Date
for(j in 1:NROW(spending)){
  spending$Long_Date[j] = ""
  for(i in 1:nchar(spending$Description[j])){
    chunk = substr(spending$Description[j], i, i+9)
    if(substr(chunk, 1, 1) == " " & substr(chunk, 4, 4) == "-" & substr(chunk, 10,10) == " "){
      spending$Long_Date[j] <- chunk
    }
  }
}

# format original date to have leading 0s
spending$Date <- format(as.Date(spending$Date, "%m/%d/%Y"), "%m/%d/%Y")

# Create column New_Date - NVL logic: Long_Date > Short_Date > Date
for(i in 1:NROW(spending)){
  if(spending$Long_Date[i]!=""){
    spending$New_Date[i] <- spending$Long_Date[i]
  } else if(spending$Short_Date[i]!=""){
    spending$New_Date[i] <- spending$Short_Date[i]
  } else {
    spending$New_Date[i] <- spending$Date[i]
  }
}

# formatting New_Date
spending$New_Date <- gsub("-", "/", spending$New_Date)
spending$New_Date <- gsub(" ", "", spending$New_Date)

# create a function that gets the last n characters of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# if New_Date doesn't have a year, take year from original Date
for(i in 1:NROW(spending)){
  if(nchar(spending$New_Date[i])==5){
    spending$New_Date[i] <- paste0(spending$New_Date[i], substrRight(spending$Date[i], 5))
  }
}

# if New_Date year is 2 characters, insert "20" into year
# this doesn't work because the dates from original Date didn't have leading 0s
for(i in 1:NROW(spending)){
  if(nchar(spending$New_Date[i])==8){
    spending$New_Date[i] <- paste0(substr(spending$New_Date[i], 1, 6), "20", substr(spending$New_Date[i], 7, 8))
  }
}

# Issue: when year is extracted from original date when not included in the description-extracted date, the year might be wrong at
# the beginning of the year 
# correct incorrect years

spending$New_Date <- as.Date(spending$New_Date, "%m/%d/%Y")
spending$Date <- as.Date(spending$Date, "%m/%d/%Y")

for(i in 1:NROW(spending)){
  if(month(spending$New_Date[i])==12 & month(spending$Date[i])==01 & year(spending$New_Date[i])==year(spending$Date[i])){
    spending$New_Date[i] <- spending$New_Date[i]-365
  }
}

# Create column Weekday
spending$Weekday <- weekdays(spending$New_Date)
spending$Weekday <- factor(spending$Weekday, 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Create column Month

## using forcats
spending$Month <- months(spending$New_Date)
spending$Month <- factor(spending$Month, levels = c("January","February", "March", "April", "May","June", "July", "August",
                                                    "September", "October", "November", "December"))

# Remove rows in current month
spending <- spending%>%
  filter(New_Date < Latest) %>%
  filter(New_Date >= Earliest)

# drop unnecessary columns
spending <- spending %>%
  select(Date=New_Date, Amount, Description, Purchase, Category, Type, Weekday, Month, City, State, Payment=Payment_Method) %>%
  setorder(-Date)

# Check for nulls after running the script once
# nulls <- filter(spending, is.na(Type))
# write.csv(nulls, "Nulls.csv")

####################################################################################################################################

################################################### PIVOT TABLES ###################################################################

### Create Table Total Spending by Category
# First, exclude the latest month as it is incomplete and will make averages per month misleading
latest_month <- months(max(spending$Date))

spending_full_months <- spending %>%
  filter(Month != latest_month)

Ordered_Spending <- spending_full_months %>%
  group_by(Category)%>%
  summarise(Total=sum(Amount)) %>%
  mutate(Average = Total/sum(table(unique(spending_full_months$Month)))) %>%
  select(Category, Average) %>%
  setorder(Average)

rm(spending_full_months)

# turn Category into a factor column according to the ordered spending table order
# this will cause tables broken out by Category to order based on average spending, highest to lowest
spending$Category <- factor(spending$Category, levels = Ordered_Spending$Category)

# Pivot: Monthly Bills

# We need to filter to the categories in Type=MONTHLY BILLS because if we filter on type before the spread, we lose months with no monthly bills yet
Monthly_Bills <- spending %>%
  #  filter(Type=="MONTHLY BILLS")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0) %>%
  filter(Category %in% c("RENT UTILITIES", "PHONE",  "STUDENT LOANS GRAD SCHOOL", "MEDIA", "INTERNET"))

#### Create table for Day-To-Day purchases

# Pivot: Day-to-Day Spending
Daily_Spending <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

#### Create table for Provisional purchases

# Pivot: Provisional Spending

# We need to filter to the categories in Type=Ad hoc because if we filter on type before the spread, we lose months with no provisional
# spending. Since other spending types have higher frequencies, they shouldnt have the same issues.

Provisional_Spending <- spending %>%
  #  filter(Type=="AD HOC PURCHASES")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0) %>%
  filter(Category %in% c("CLOTHES SHOES", "TRAVEL", "ELECTRONICS MOVIES BOOKS", "GIFTS HOLIDAYS", 
                         "HOUSEHOLD ITEMS", "DONATIONS", "ONLINE PURCHASES"))

####### Total Net Spending

Net_Spending_Savings <- spending %>%
  group_by(Month, Type)%>%
  summarise(Total=sum(Amount))%>%
  spread(Month, Total, fill=0)

Net_Spending <- Net_Spending_Savings[1:5,]
savings_investments <- Net_Spending_Savings[6,]

Total_Spending <- spending %>%
  filter(Type!="SAVINGS INVESTMENTS", Type!="INCOME") %>%
  group_by(Month)%>%
  summarise(Total=sum(Amount))%>%
  spread(Month, Total, fill=0)

####### Total Income

Total_Income <- Net_Spending_Savings[1,]

rm(Net_Spending_Savings)

# write.csv(spending, file = "Spending_Data.csv")
# save(spending, file = "spending_data.rda")

###############################################################################################################################

######## EXCEL #########

# Daily_Spending_Cat_Count
# Spending_by_Weekday
# Top_20_Purchases_Amount
# Top_20_Purchases_Count

## Create styles

Title <- createStyle(halign = "left", valign = "center", textDecoration = "bold")

Headers <- createStyle(fontColour = "#FFFFFF", border = c("top", "bottom", "left", "right"), 
                       fgFill = "#6495ED", halign = "center", valign = "center",
                       textDecoration = "bold")

Numbers <- createStyle(numFmt = '_($* #,##0_);_($* (#,##0);_($* "-"??_);_(@_)', border = c("top", "bottom", "left", "right"),
                       halign = "right", valign = "center")

Total <- createStyle(border = c("top", "bottom", "left", "right"), 
                     fgFill = "#C0C0C0", halign = "center", valign = "center")

Double_Border <- createStyle(border = "bottom", borderStyle = "double")

## Create a new workbook

wb <- createWorkbook(creator = "Thomas"
                     , title = "Monthly Spending Report"
                     , subject = "June 2019 to Present Spending Breakdown")

addWorksheet(wb, "Spending Summary")

# FOOTNOTES
min_date <- format(as.Date(min(spending$Date), "%Y-%m-%d"), "%m/%d/%Y")
max_date <- format(as.Date(max(spending$Date), "%Y-%m-%d"), "%m/%d/%Y")

footnote <- paste0("This report includes transactions occurring from ", min_date, " through ", max_date, ". The month of ", latest_month, 
                   " is not complete. Averages only include complete months.")

writeData(wb,"Spending Summary", footnote, startCol = 1, startRow = 1)

# Table: Spending Summary AND INCOME BY TYPE
writeData(wb,"Spending Summary", "ALL SPENDING AND INCOME (AFTER TAX)", startCol = 5, startRow = 2)

writeData(wb,"Spending Summary", Total_Income, startCol = 5, startRow = 3, borders = "all")
writeData(wb,"Spending Summary", "TOTAL SPENDING", startCol = 5, startRow = 5, borders = "all")
writeData(wb,"Spending Summary", Total_Spending, startCol = 6, startRow = 5, colNames = FALSE, borders = "all")
writeData(wb,"Spending Summary", Net_Spending[2:5,], startCol = 5, startRow = 6, colNames = FALSE, borders = "all")
conditionalFormatting(wb, "Spending Summary", cols=6:(5+length(Total_Spending)), rows=4:9, style = c("#F8696B", "#FFEB84", "#63BE7B"), type = "colourScale")

# SAVINGS INVESTMENTS
writeData(wb,"Spending Summary", "SAVINGS AND INVESTMENTS", startCol = 5, startRow = 11, borders = "all")
addStyle(wb, "Spending Summary", Title, rows=11, cols=5)
writeData(wb,"Spending Summary", savings_investments, startCol = 5, startRow = 12, borders = "all")
addStyle(wb, "Spending Summary", Headers, cols=c(5:(5+length(Total_Spending))), rows=12)
addStyle(wb, "Spending Summary", Numbers, rows=13, cols=6:(5+length(Total_Spending)))
conditionalFormatting(wb, "Spending Summary", cols=6:(5+length(Total_Spending)), rows=13, style = c("#63BE7B", "#FFEB84", "#F8696B"), type = "colourScale")

# Table: AVERAGE SPENDING AND INCOME BY CATEGORY
writeData(wb,"Spending Summary", "AVERAGE MONTHLY SPENDING AND INCOME", startCol = 2, startRow = 2)
writeData(wb,"Spending Summary", Ordered_Spending, startCol = 2, startRow = 3, borders = "all")
conditionalFormatting(wb, "Spending Summary", cols=3, rows=4:27, style = c("#F8696B", "#FFEB84", "#63BE7B"), type = "colourScale")

# Add Styles
addStyle(wb, "Spending Summary", Title, rows=c(2,2), cols=c(2,5), gridExpand = FALSE, stack = FALSE)
addStyle(wb, "Spending Summary", Headers, cols=c(2:3), rows=c(3), gridExpand = FALSE, stack = FALSE)
addStyle(wb, "Spending Summary", Headers, cols=c(5:(5+length(Total_Spending))), rows=c(3), gridExpand = FALSE, stack = FALSE)
addStyle(wb, "Spending Summary", Numbers, rows=4:9, cols=6:(5+length(Total_Spending)), gridExpand = TRUE, stack = FALSE)
addStyle(wb, "Spending Summary", Numbers, rows=c(4:27), cols=c(3), gridExpand = FALSE, stack = FALSE)
addStyle(wb, "Spending Summary", Total, rows=5, cols=5, gridExpand = FALSE, stack = FALSE)
addStyle(wb, "Spending Summary", Double_Border, rows=5, cols=5:(5+length(Total_Spending)), gridExpand = FALSE, stack = TRUE)

# Table: MONTHLY BILLS
writeData(wb,"Spending Summary", "MONTHLY BILLS", startCol = 5, startRow = 15)
writeData(wb,"Spending Summary", Monthly_Bills, startCol = 5, startRow = 16, borders = "all")
addStyle(wb, "Spending Summary", Title, rows=15, cols=5)
addStyle(wb, "Spending Summary", Headers, rows=16, cols=5:(5+length(Total_Spending)))
addStyle(wb, "Spending Summary", Numbers, rows=17:21, cols=6:(5+length(Total_Spending)), gridExpand = TRUE)
conditionalFormatting(wb, "Spending Summary", rows=17:21, cols=6:(5+length(Total_Spending)), style = c("#F8696B", "#FFEB84"), type = "colourScale")

# Table: DAILY SPENDING
writeData(wb,"Spending Summary", "DAY TO DAY SPENDING", startCol = 5, startRow = 23)
writeData(wb,"Spending Summary", Daily_Spending, startCol = 5, startRow = 24, borders = "all")
addStyle(wb, "Spending Summary", Title, rows=23, cols=5)
addStyle(wb, "Spending Summary", Headers, rows=24, cols=5:(5+length(Total_Spending)))
addStyle(wb, "Spending Summary", Numbers, rows=25:32, cols=6:(5+length(Total_Spending)), gridExpand = TRUE)
conditionalFormatting(wb, "Spending Summary", rows=25:32, cols=6:(5+length(Total_Spending)), style = c("#F8696B", "#FFEB84"), type = "colourScale")

# Table: PROVISIONAL SPENDING
# Table: DAILY SPENDING
writeData(wb,"Spending Summary", "AD HOC SPENDING", startCol = 5, startRow = 34)
writeData(wb,"Spending Summary", Provisional_Spending, startCol = 5, startRow = 35, borders = "all")
addStyle(wb, "Spending Summary", Title, rows=34, cols=5)
addStyle(wb, "Spending Summary", Headers, rows=35, cols=5:(5+length(Total_Spending)))
addStyle(wb, "Spending Summary", Numbers, rows=36:41, cols=6:(5+length(Total_Spending)), gridExpand = TRUE)
conditionalFormatting(wb, "Spending Summary", rows=36:41, cols=6:(5+length(Total_Spending)), style = c("#F8696B", "#FFEB84"), type = "colourScale")


## Tab column widths
setColWidths(wb, "Spending Summary", cols=2, widths = 31)
setColWidths(wb, "Spending Summary", cols=5, widths = 24)
setColWidths(wb, "Spending Summary", cols=c(3,6:(5+length(Total_Spending))), widths = 12)

# Gridlines
showGridLines(wb, "Spending Summary", showGridLines = FALSE)

### Save the workbook
#saveWorkbook(wb, "../Monthly Spending.xlsx", overwrite = TRUE)

saveWorkbook(wb, paste0("../Spending Report ", gsub("/", "-", min_date), " To ", gsub("/", "-", max_date), ".xlsx"), overwrite = TRUE)

### Plotting
ggplot(spending, aes(x=Month, y=Amount, fill=Type)) + geom_bar(stat="identity")