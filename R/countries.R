#ISO 3166-1 alpha-2 codes of the countries that sources name rather than code,
#such as xeno-canto's cnt (Spain, Russian Federation). The table holds the ISO
#3166-1 English short names, the shortenings of them that only one country has
#(Bolivia for "Bolivia (Plurinational State of)", Tanzania for "Tanzania,
#United Republic of"), those names without their and (Bosnia Herzegovina,
#Trinidad Tobago), which is how xeno-canto writes them, and the names in
#common use that none of those covers (United States, Laos, Ivory Coast, and
#the two Congos, which xeno-canto tells apart in brackets). A shortening that
#two countries share (Congo, Korea, Virgin Islands) is left out, as it names
#neither of them.
#
#The names are ASCII, so a source writing Reunion or Curacao with the accents
#they have in ISO 3166-1 is not read. countryOfName() leaves such a name
#unread rather than guessing at it, and it can be added here.
#
#Checked on 2026-09-20 against the 133 countries a sample of 6000 xeno-canto
#recordings from every area names: all of them were read.
countryNames <- c(
  "afghanistan"="AF", "aland islands"="AX", "albania"="AL", "algeria"="DZ",
  "american samoa"="AS", "andorra"="AD", "angola"="AO", "anguilla"="AI", "antarctica"="AQ",
  "antigua and barbuda"="AG", "antigua barbuda"="AG", "argentina"="AR", "armenia"="AM",
  "aruba"="AW", "australia"="AU", "austria"="AT", "azerbaijan"="AZ", "bahamas"="BS",
  "bahrain"="BH", "bangladesh"="BD", "barbados"="BB", "belarus"="BY", "belgium"="BE",
  "belize"="BZ", "benin"="BJ", "bermuda"="BM", "bhutan"="BT", "bolivia"="BO",
  "bolivia, plurinational state of"="BO", "bonaire"="BQ",
  "bonaire, sint eustatius and saba"="BQ", "bonaire, sint eustatius saba"="BQ",
  "bosnia and herzegovina"="BA", "bosnia herzegovina"="BA", "botswana"="BW",
  "bouvet island"="BV", "brazil"="BR", "british indian ocean territory"="IO", "brunei"="BN",
  "brunei darussalam"="BN", "bulgaria"="BG", "burkina faso"="BF", "burma"="MM",
  "burma (myanmar)"="MM", "burundi"="BI", "cabo verde"="CV", "cambodia"="KH", "cameroon"="CM",
  "canada"="CA", "cape verde"="CV", "cayman islands"="KY", "central african republic"="CF",
  "chad"="TD", "chile"="CL", "china"="CN", "christmas island"="CX", "cocos"="CC",
  "cocos (keeling) islands"="CC", "colombia"="CO", "comoros"="KM", "congo (brazzaville)"="CG",
  "congo (democratic republic)"="CD", "congo (kinshasa)"="CD", "congo (republic)"="CG",
  "congo, democratic republic of the"="CD", "congo-brazzaville"="CG", "congo-kinshasa"="CD",
  "cook islands"="CK", "costa rica"="CR", "cote d'ivoire"="CI", "croatia"="HR", "cuba"="CU",
  "curacao"="CW", "cyprus"="CY", "czech republic"="CZ", "czechia"="CZ",
  "democratic republic of the congo"="CD", "denmark"="DK", "djibouti"="DJ", "dominica"="DM",
  "dominican republic"="DO", "dr congo"="CD", "drc"="CD", "east timor"="TL", "ecuador"="EC",
  "egypt"="EG", "el salvador"="SV", "england"="GB", "equatorial guinea"="GQ", "eritrea"="ER",
  "estonia"="EE", "eswatini"="SZ", "ethiopia"="ET", "falkland islands"="FK",
  "falkland islands (malvinas)"="FK", "faroe islands"="FO", "fiji"="FJ", "finland"="FI",
  "france"="FR", "french guiana"="GF", "french polynesia"="PF",
  "french southern territories"="TF", "gabon"="GA", "gambia"="GM", "georgia"="GE",
  "germany"="DE", "ghana"="GH", "gibraltar"="GI", "great britain"="GB", "greece"="GR",
  "greenland"="GL", "grenada"="GD", "guadeloupe"="GP", "guam"="GU", "guatemala"="GT",
  "guernsey"="GG", "guinea"="GN", "guinea-bissau"="GW", "guyana"="GY", "haiti"="HT",
  "heard island and mcdonald islands"="HM", "heard island mcdonald islands"="HM",
  "holy see"="VA", "honduras"="HN", "hong kong"="HK", "hong kong sar china"="HK",
  "hungary"="HU", "iceland"="IS", "india"="IN", "indonesia"="ID", "iran"="IR",
  "iran, islamic republic of"="IR", "iraq"="IQ", "ireland"="IE", "isle of man"="IM",
  "israel"="IL", "italy"="IT", "ivory coast"="CI", "jamaica"="JM", "japan"="JP", "jersey"="JE",
  "jordan"="JO", "kazakhstan"="KZ", "kenya"="KE", "kiribati"="KI",
  "korea, democratic people's republic of"="KP", "korea, republic of"="KR", "kuwait"="KW",
  "kyrgyzstan"="KG", "lao people's democratic republic"="LA", "laos"="LA", "latvia"="LV",
  "lebanon"="LB", "lesotho"="LS", "liberia"="LR", "libya"="LY", "liechtenstein"="LI",
  "lithuania"="LT", "luxembourg"="LU", "macao"="MO", "macau"="MO", "macedonia"="MK",
  "madagascar"="MG", "malawi"="MW", "malaysia"="MY", "maldives"="MV", "mali"="ML",
  "malta"="MT", "marshall islands"="MH", "martinique"="MQ", "mauritania"="MR",
  "mauritius"="MU", "mayotte"="YT", "mexico"="MX", "micronesia"="FM",
  "micronesia, federated states of"="FM", "moldova"="MD", "moldova, republic of"="MD",
  "monaco"="MC", "mongolia"="MN", "montenegro"="ME", "montserrat"="MS", "morocco"="MA",
  "mozambique"="MZ", "myanmar"="MM", "namibia"="NA", "nauru"="NR", "nepal"="NP",
  "netherlands"="NL", "netherlands antilles"="AN", "netherlands, kingdom of the"="NL",
  "new caledonia"="NC", "new zealand"="NZ", "nicaragua"="NI", "niger"="NE", "nigeria"="NG",
  "niue"="NU", "norfolk island"="NF", "north korea"="KP", "north macedonia"="MK",
  "northern ireland"="GB", "northern mariana islands"="MP", "norway"="NO", "oman"="OM",
  "pakistan"="PK", "palau"="PW", "palestine"="PS", "palestine, state of"="PS", "panama"="PA",
  "papua new guinea"="PG", "paraguay"="PY", "peru"="PE", "philippines"="PH", "pitcairn"="PN",
  "pitcairn islands"="PN", "poland"="PL", "portugal"="PT", "puerto rico"="PR", "qatar"="QA",
  "republic of korea"="KR", "republic of the congo"="CG", "reunion"="RE", "romania"="RO",
  "russia"="RU", "russian federation"="RU", "rwanda"="RW", "saint barthelemy"="BL",
  "saint helena"="SH", "saint helena, ascension and tristan da cunha"="SH",
  "saint helena, ascension tristan da cunha"="SH", "saint kitts and nevis"="KN",
  "saint kitts nevis"="KN", "saint lucia"="LC", "saint martin"="MF",
  "saint martin (french part)"="MF", "saint pierre and miquelon"="PM",
  "saint pierre miquelon"="PM", "saint vincent and the grenadines"="VC",
  "saint vincent the grenadines"="VC", "samoa"="WS", "san marino"="SM",
  "sao tome and principe"="ST", "sao tome principe"="ST", "saudi arabia"="SA", "scotland"="GB",
  "senegal"="SN", "serbia"="RS", "seychelles"="SC", "sierra leone"="SL", "singapore"="SG",
  "sint maarten"="SX", "sint maarten (dutch part)"="SX", "slovakia"="SK", "slovenia"="SI",
  "solomon islands"="SB", "somalia"="SO", "south africa"="ZA",
  "south georgia and the south sandwich islands"="GS",
  "south georgia the south sandwich islands"="GS", "south korea"="KR", "south sudan"="SS",
  "spain"="ES", "sri lanka"="LK", "sudan"="SD", "suriname"="SR", "svalbard and jan mayen"="SJ",
  "svalbard jan mayen"="SJ", "swaziland"="SZ", "sweden"="SE", "switzerland"="CH", "syria"="SY",
  "syrian arab republic"="SY", "taiwan"="TW", "taiwan, province of china"="TW",
  "tajikistan"="TJ", "tanzania"="TZ", "tanzania, united republic of"="TZ", "thailand"="TH",
  "the bahamas"="BS", "the gambia"="GM", "timor-leste"="TL", "togo"="TG", "tokelau"="TK",
  "tonga"="TO", "trinidad and tobago"="TT", "trinidad tobago"="TT", "tunisia"="TN",
  "turkey"="TR", "turkiye"="TR", "turkmenistan"="TM", "turks and caicos islands"="TC",
  "turks caicos islands"="TC", "tuvalu"="TV", "uganda"="UG", "uk"="GB", "ukraine"="UA",
  "united arab emirates"="AE", "united kingdom"="GB",
  "united kingdom of great britain and northern ireland"="GB",
  "united kingdom of great britain northern ireland"="GB", "united states"="US",
  "united states minor outlying islands"="UM", "united states of america"="US", "uruguay"="UY",
  "usa"="US", "uzbekistan"="UZ", "vanuatu"="VU", "vatican"="VA", "vatican city"="VA",
  "venezuela"="VE", "venezuela, bolivarian republic of"="VE", "viet nam"="VN", "vietnam"="VN",
  "virgin islands (british)"="VG", "virgin islands (u.s.)"="VI", "wales"="GB",
  "wallis and futuna"="WF", "wallis and futuna islands"="WF", "wallis futuna"="WF",
  "western sahara"="EH", "yemen"="YE", "zaire"="CD", "zambia"="ZM", "zimbabwe"="ZW"
)

#ISO 3166-1 alpha-2 codes of the names of countries, matched whatever their
#case and spacing; NA for a name that isn't a country's, which is how a source
#that gives a region or a place rather than a country is left unread
countryOfName <- function(x) {
  name <- tolower(gsub("\\s+", " ", trimws(as.character(x))))
  return(unname(countryNames[match(name, names(countryNames))]))
}
