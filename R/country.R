#Reads the names sources give countries as ISO 3166-1 alpha-2 codes, which is
#what the countryCode and country columns hold (see normaliseRecordings() and
#normaliseSpecimens()). Darwin Core's country is a name, not a code, so a
#source that gives Darwin Core needs its names read before upload.
#
#Names are matched without case, accents or repeated spaces, so Cote d'Ivoire
#and Cote d'Ivoire are one name. A name that isn't a country's is NA, which is
#uploaded as NULL: a country that can't be read is left out rather than
#guessed.

#The English short names of ISO 3166-1, by their alpha-2 code
countryNames <- c(
  AD="Andorra", AE="United Arab Emirates", AF="Afghanistan",
  AG="Antigua and Barbuda", AI="Anguilla", AL="Albania", AM="Armenia",
  AO="Angola", AQ="Antarctica", AR="Argentina", AS="American Samoa",
  AT="Austria", AU="Australia", AW="Aruba", AX="Aland Islands",
  AZ="Azerbaijan", BA="Bosnia and Herzegovina", BB="Barbados",
  BD="Bangladesh", BE="Belgium", BF="Burkina Faso", BG="Bulgaria",
  BH="Bahrain", BI="Burundi", BJ="Benin", BL="Saint Barthelemy",
  BM="Bermuda", BN="Brunei Darussalam", BO="Bolivia (Plurinational State of)",
  BQ="Bonaire, Sint Eustatius and Saba", BR="Brazil", BS="Bahamas",
  BT="Bhutan", BV="Bouvet Island", BW="Botswana", BY="Belarus", BZ="Belize",
  CA="Canada", CC="Cocos (Keeling) Islands",
  CD="Congo, Democratic Republic of the", CF="Central African Republic",
  CG="Congo", CH="Switzerland", CI="Cote d'Ivoire", CK="Cook Islands",
  CL="Chile", CM="Cameroon", CN="China", CO="Colombia", CR="Costa Rica",
  CU="Cuba", CV="Cabo Verde", CW="Curacao", CX="Christmas Island",
  CY="Cyprus", CZ="Czechia", DE="Germany", DJ="Djibouti", DK="Denmark",
  DM="Dominica", DO="Dominican Republic", DZ="Algeria", EC="Ecuador",
  EE="Estonia", EG="Egypt", EH="Western Sahara", ER="Eritrea", ES="Spain",
  ET="Ethiopia", FI="Finland", FJ="Fiji", FK="Falkland Islands (Malvinas)",
  FM="Micronesia (Federated States of)", FO="Faroe Islands", FR="France",
  GA="Gabon", GB="United Kingdom of Great Britain and Northern Ireland",
  GD="Grenada", GE="Georgia", GF="French Guiana", GG="Guernsey", GH="Ghana",
  GI="Gibraltar", GL="Greenland", GM="Gambia", GN="Guinea", GP="Guadeloupe",
  GQ="Equatorial Guinea", GR="Greece",
  GS="South Georgia and the South Sandwich Islands", GT="Guatemala",
  GU="Guam", GW="Guinea-Bissau", GY="Guyana", HK="Hong Kong",
  HM="Heard Island and McDonald Islands", HN="Honduras", HR="Croatia",
  HT="Haiti", HU="Hungary", ID="Indonesia", IE="Ireland", IL="Israel",
  IM="Isle of Man", IN="India", IO="British Indian Ocean Territory",
  IQ="Iraq", IR="Iran (Islamic Republic of)", IS="Iceland", IT="Italy",
  JE="Jersey", JM="Jamaica", JO="Jordan", JP="Japan", KE="Kenya",
  KG="Kyrgyzstan", KH="Cambodia", KI="Kiribati", KM="Comoros",
  KN="Saint Kitts and Nevis",
  KP="Korea (Democratic People's Republic of)", KR="Korea, Republic of",
  KW="Kuwait", KY="Cayman Islands", KZ="Kazakhstan",
  LA="Lao People's Democratic Republic", LB="Lebanon", LC="Saint Lucia",
  LI="Liechtenstein", LK="Sri Lanka", LR="Liberia", LS="Lesotho",
  LT="Lithuania", LU="Luxembourg", LV="Latvia", LY="Libya", MA="Morocco",
  MC="Monaco", MD="Moldova, Republic of", ME="Montenegro",
  MF="Saint Martin (French part)", MG="Madagascar", MH="Marshall Islands",
  MK="North Macedonia", ML="Mali", MM="Myanmar", MN="Mongolia", MO="Macao",
  MP="Northern Mariana Islands", MQ="Martinique", MR="Mauritania",
  MS="Montserrat", MT="Malta", MU="Mauritius", MV="Maldives", MW="Malawi",
  MX="Mexico", MY="Malaysia", MZ="Mozambique", "NA"="Namibia",
  NC="New Caledonia", NE="Niger", NF="Norfolk Island", NG="Nigeria",
  NI="Nicaragua", NL="Netherlands", NO="Norway", NP="Nepal", NR="Nauru",
  NU="Niue", NZ="New Zealand", OM="Oman", PA="Panama", PE="Peru",
  PF="French Polynesia", PG="Papua New Guinea", PH="Philippines",
  PK="Pakistan", PL="Poland", PM="Saint Pierre and Miquelon", PN="Pitcairn",
  PR="Puerto Rico", PS="Palestine, State of", PT="Portugal", PW="Palau",
  PY="Paraguay", QA="Qatar", RE="Reunion", RO="Romania", RS="Serbia",
  RU="Russian Federation", RW="Rwanda", SA="Saudi Arabia",
  SB="Solomon Islands", SC="Seychelles", SD="Sudan", SE="Sweden",
  SG="Singapore", SH="Saint Helena, Ascension and Tristan da Cunha",
  SI="Slovenia", SJ="Svalbard and Jan Mayen", SK="Slovakia",
  SL="Sierra Leone", SM="San Marino", SN="Senegal", SO="Somalia",
  SR="Suriname", SS="South Sudan", ST="Sao Tome and Principe",
  SV="El Salvador", SX="Sint Maarten (Dutch part)", SY="Syrian Arab Republic",
  SZ="Eswatini", TC="Turks and Caicos Islands", TD="Chad",
  TF="French Southern Territories", TG="Togo", TH="Thailand",
  TJ="Tajikistan", TK="Tokelau", TL="Timor-Leste", TM="Turkmenistan",
  TN="Tunisia", TO="Tonga", TR="Turkiye", TT="Trinidad and Tobago",
  TV="Tuvalu", TW="Taiwan, Province of China",
  TZ="Tanzania, United Republic of", UA="Ukraine", UG="Uganda",
  UM="United States Minor Outlying Islands", US="United States of America",
  UY="Uruguay", UZ="Uzbekistan", VA="Holy See",
  VC="Saint Vincent and the Grenadines",
  VE="Venezuela (Bolivarian Republic of)", VG="Virgin Islands (British)",
  VI="Virgin Islands (U.S.)", VN="Viet Nam", VU="Vanuatu",
  WF="Wallis and Futuna", WS="Samoa", YE="Yemen", YT="Mayotte",
  ZA="South Africa", ZM="Zambia", ZW="Zimbabwe")

#The other names sources give those countries: the names of the United Nations
#(which TaxonWorks uses), the everyday names, and names that have changed
countryOtherNames <- c(
  BO="Bolivia", BN="Brunei", CD="Democratic Republic of the Congo",
  CG="Republic of the Congo", CV="Cape Verde", CI="Ivory Coast",
  CZ="Czech Republic", FK="Falkland Islands", FM="Micronesia",
  GB="United Kingdom", IR="Iran", KP="North Korea",
  KP="Democratic People's Republic of Korea", KR="South Korea",
  KR="Republic of Korea", LA="Laos", MD="Moldova", MD="Republic of Moldova",
  MK="Macedonia", MK="The former Yugoslav Republic of Macedonia",
  MM="Burma", PS="Palestine", PS="State of Palestine", RU="Russia",
  SH="Saint Helena", SY="Syria", SZ="Swaziland", TL="East Timor",
  TR="Turkey", TW="Taiwan", TZ="United Republic of Tanzania",
  TZ="Tanzania", US="United States", VA="Vatican City",
  VE="Venezuela", VG="British Virgin Islands",
  VI="United States Virgin Islands", VN="Vietnam")

#ISO 3166-1 alpha-2 codes of the names of countries, whether they are written
#with accents or not and in whatever case; NA for anything that isn't a
#country's name. A code that is already a code is left as it is.
#' @importFrom stringi stri_trans_general
countryName2Code <- function(x) {
  named <- c(countryNames, countryOtherNames)
  codes <- stats::setNames(names(named), countryKey(unname(named)))
  out <- unname(codes[countryKey(x)])
  #A source that gives a code rather than a name already has one
  return(ifelse(is.na(out), countryCode(x), out))
}

#The form country names are matched in: without case, accents or repeated
#spaces, so that Cote d'Ivoire matches its accented spelling
countryKey <- function(x) {
  x <- stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
  return(tolower(gsub("[[:space:]]+", " ", x)))
}
