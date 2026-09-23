# q14_sector_coding.R — first-pass coding of the open-ended Q14 "which sector
# do you work in?" answers into NACE divisions + decarbonisation job-loss
# exposure tiers, for the E5 (high-carbon worker) subgroup analysis.
#
# Output: data/q14_sector_coded.csv — one row per (Country, normalized_key)
#   raw            representative (most frequent) original string for the key
#   normalized_key Layer-1 normalised string (typography only, meaning-preserving)
#   country        Spain / France / Germany / Romania
#   freq           number of respondents with this key
#   nace_div       suggested NACE Rev.2 division (letter), first-pass
#   sector_en      short English sector label
#   tier           High / Medium / Low / Unclassifiable  ("" = unmatched, review)
#   flags          semicolon-separated: multi; broad; review; rail_exception; occupation
#
# tier → E5 definitions (derive later): narrow high-C = {High}; broad = {High, Medium}.
# First pass only: unmatched (tier == "") and any flagged row is for human validation.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(tidyr)
})

COUNTRY_FILES <- c(
  Spain   = "data/Spanish_5.+Mai+2026_15.02.csv",
  France  = "data/French_9.+März+2026_09.31.csv",
  Germany = "data/German_9.+März+2026_09.31.csv",
  Romania = "data/Romanian_5.+Mai+2026_15.02.csv"
)
STATUS_IP <- c("IP-Adresse", "IP Address")

# ---- Layer 1: string normalisation (typography only) -------------------------
# normalize_key() is defined in R/subgroup_covariates_helpers.R so the coding
# side and the respondent-merge side use the identical function.
source("R/subgroup_covariates_helpers.R")

# Noise / non-sector tokens (agreed: route to Unclassifiable, flagged; not deleted).
NOISE_PATTERNS <- paste(
  "^$",
  "^n ?/? ?a$", "^na$", "^-+$", "^\\.*$", "^[0-9]+$",
  "temps plein", "temps partiel", "plein temps", "mi temps",
  "^prive$", "^privee?$", "secteur prive", "^public$", "^privad", # bare public/private
  "retraite", "retired", "jubilad", "rentner", "pensionar",
  "etudiant", "student", "estudiant", "eleve", "\\bbts\\b", "\\bdut\\b", "\\bdeug\\b",
  "chomage", "chomeu", "sans emploi", "en recherche", "recherche d emploi",
  "je cherche", "cherche un", "desemplea", "arbeitslos", "somaj", "somer",
  "prefer", "prefiero no", "^sans$", "^aucun", "keine angabe", "\\bnsp\\b",
  "salarie$", "^employe", "^employee$", "^angestellt", "\\bofficer\\b",
  "^no trabajo", "^ningun", "^nimic", "ama de casa", "femme au foyer",
  "hausfrau", "^casnic", "^test$", "^prueba$", "^rien$",
  "^muncitor", "^ouvrier", "^arbeiter$", "^obrero", "^operario", "^operator$",
  "^keine$", "^nu$", "^nu am$", "^nu lucrez", "^da$", "^neant$", "^nada$",
  "^particulier$", "^desempleo$", "^rien a signaler$", "^privat$",
  sep = "|"
)

# ---- Layer 2: ordered, most-specific-first dictionary ------------------------
# Each rule: pattern (regex on normalized_key), nace_div, sector_en, tier, flag.
# FIRST match wins, so put specific/High rules and exceptions before generic ones.
RULES <- tibble::tribble(
  ~pattern,                                                                 ~nace, ~sector_en,                     ~tier,           ~flag,
  # --- exceptions that must precede generic transport / energy --------------
  "\\bsncf\\b|ferroviar|feroviar|cale ferata|railway|\\brail\\b|\\btrain|eisenbahn|\\bbahn\\b|metrou|tramway", "H49", "Rail transport",            "Low",           "rail_exception",
  "nucleair|nuclear|\\benr\\b|renouvelab|energies renouvelab|solaire|photovolta|eolien|\\bwind\\b|erneuerbar|regenerativ|energie verde|solar|fotovolta", "D35", "Low-carbon energy", "Low",  "review",
  "pharmaceut|\\bpharma\\b|pharmazie|farmaceut|farmaci|farmacie|apotheke|laboratoire pharma", "C21", "Pharmaceuticals",           "Low",           "",
  # --- HIGH: fossil / energy-intensive industry / auto ----------------------
  "mineri|minier|bergbau|\\bmine\\b|\\bmines\\b|extracti|exploatare miniera|coal|charbon|kohle|\\bcarbon\\b|carbune|carbon mineral", "B", "Mining/extraction", "High", "",
  "petrol|petrole|petroleo|raffiner|refiner|refinar|erdol|rafina|\\boil\\b|hidrocarbur|forage", "C19", "Coke & refined petroleum",  "High",          "",
  "metallurg|metalurg|siderurg|\\bacier|\\bacero|\\bstahl|\\bsteel|fonderie|foundr|giesserei|turnator|\\bmetal|\\botel\\b|aluminium|aluminum|non ferr|\\bfer\\b", "C24", "Basic metals",              "High",          "",
  "ciment|cement|cemento|zement|\\bverre\\b|\\bglass\\b|\\bglas\\b|vidrio|sticla|ceramic|ceramique|\\bchaux\\b|\\blime\\b|\\bbeton|caramida", "C23", "Non-metallic minerals",     "High",          "",
  "chimi|chemi|quimic|chemie|fertiliz|engrais|ingrasamin|plasturgi|petrochim|plastique|\\bplastic|kunststoff", "C20", "Chemicals & plastics",      "High",          "",
  "\\bpapier|\\bpaper|\\bpapel|zellstoff|celuloz|\\bpulp\\b|carton|cardboard|hartie", "C17", "Pulp & paper",              "High",          "",
  "automob|automotiv|automoci|automotriz|\\bauto\\b|voiture|\\bkfz\\b|autovehicul|constructeur automobile|equipementier", "C29", "Motor vehicles",            "High",          "",
  "aeronaut|aerospati|aerospace|luftfahrt|avion", "C30", "Aerospace manufacturing",   "Medium",        "review",
  "maschinenbau|machinery|\\bmasini\\b|constructii de masini|mecanic|mecaniqu|mecanica|mechanical eng", "C28", "Machinery & equipment",     "Medium",        "review",
  # --- MEDIUM: transport/freight, construction, agri/food, generic industry -
  "transport|logisti|\\bfret\\b|freight|camion|routier|trucking|aviation|aerien|aeropuerto|maritim|\\bnaval\\b|shipping|spedition|curier|\\bport\\b|\\bsofer\\b|chauffeur|\\blager\\b|\\bposte\\b|\\bpostal|\\bposta\\b|warehouse|depozit|verkehr|logistica", "H", "Transport & logistics",     "Medium",        "review",
  "textil|textile|croitor|confectii|confeccion|bekleidung|\\bmode\\b|\\bmoda\\b|filatur|tesatur|couture|maroquineri|\\bcuir\\b|\\bleder\\b", "C13", "Textiles & apparel",        "Medium",        "review",
  "construc|\\bbtp\\b|batiment|bauwesen|baubranche|baugewerbe|\\bbau\\b|\\bobras\\b|travaux public|santier|zidar|hochbau|tiefbau", "F", "Construction",              "Medium",        "",
  "agricultur|agricol|landwirtschaft|\\bferme\\b|\\bfarm\\b|elevage|livestock|viticultur|zootehni|\\bagro\\b|sylvicultur|foresti|forst|padure", "A", "Agriculture & forestry",    "Medium",        "",
  "alimentaire|alimentacion|alimentar|aliment|lebensmittel|nahrung|agroalim|industrie alimentaire|\\bfood\\b|\\bcarne\\b|lactat|bautur|brasserie|boisson|boulanger|backerei|panific|patisser", "C10", "Food & beverages",          "Medium",        "",
  "energie|energy|energia|energetic|\\bgaz\\b|\\bgas\\b|centrale|kraftwerk|power plant|utilit|electricit|\\bstrom\\b|elektrizit|energieversorg", "D35", "Energy (unspecified)",      "Medium",        "review",
  "industri|fabricat|fabricac|fabricar|manufactur|\\busine\\b|\\bfabrik|fertigung|\\bproducti|produccion|productie|produktio|produktion|\\bproductio", "C", "Manufacturing (generic)",   "Medium",        "review",
  "dechet|\\bwaste\\b|abfall|recycl|assainiss|\\beau\\b|\\bwater\\b|\\bapa\\b|salubr|entsorgung|deseuri", "E", "Water & waste",             "Medium",        "",
  # --- LOW: public, education, health, retail, finance, IT, services --------
  "fonction publiqu|fonctionnair|funcionari|functionar|secteur public|sector public|sector publico|service public|\\bpublico\\b|\\bmairie\\b|\\bcommune\\b|collectivite|gouvernement|government|gobierno|\\bstaat|offentlich|offentliche verwaltung|verwaltung|behorde|administrati|administracion|administration|prefectur|primaria|\\betat\\b|ministe|\\bmilitair|\\barmee\\b|\\barmy\\b|armata|\\bmilit|gendarmeri|\\bpolice\\b|politie|politia|\\bjustiz\\b|justice|defens|douane", "O84", "Public administration",     "Low",           "broad",
  "educaci|educati|education|enseign|\\becole|escuela|schule|bildung|universit|\\buni\\b|\\blehrer|\\bteacher|profesor|invatamant|formation|erzieh|pedagog|padagog|petite enfance|assistante maternel|kinderbetreu|crche|creche|gradinita", "P85", "Education & childcare",     "Low",           "",
  "\\bsante\\b|\\bsalud\\b|sanidad|sanitar|sanatat|\\bhealth|gesundheit|\\bpflege|altenpfleg|\\bmedic|medizin|hopita|hospital|krankenhaus|\\bsoins\\b|infirm|\\bnurse|\\bspital|dental|ehpad|aide a domicile|aide a la personne|aide menager|service a la personne|asistent personal|asistenta sociala|sozialwesen|sozialarbeit|soziale arbeit|\\bsoziales\\b|\\bsocial\\b|\\bong\\b|\\badmr\\b|caritati|association|associatif", "Q", "Health & social care",      "Low",           "",
  "\\bde gros\\b|\\bgross|wholesale|\\bgrossist", "G46", "Wholesale trade",           "Low",           "",
  "commerc|comerci|comert|\\bretail\\b|\\bvente|\\bventas|\\bvanzator|verkauf|einzelhandel|\\bhandel|vanzari|vinzari|magasin|\\bshop\\b|supermarch|supermarket|supermercad|distribution|\\bkasse", "G47", "Retail trade",              "Low",           "",
  "banqu|\\bbanc|\\bbank|\\bfinanc|finant|finanzas|finanzen|\\bassuranc|versicherung|\\bseguros|asigurar|comptab|contabil|contabilidad|accounting|buchhalt|\\bfiscal|\\beconomist|economi|\\bimpot", "K", "Finance & insurance",       "Low",           "",
  "informatiqu|\\bit\\b|software|logiciel|informatic|tecnologi|tehnolog|technolog|\\btech\\b|\\bedv\\b|\\bict\\b|telecom|telekommunik|programm|developp|developer|reseau", "J", "IT & telecom",              "Low",           "occupation",
  "hoteler|hoteli|hotell|hosteleri|restaurac|restauratio|restauration|\\brestaurant|gastronomi|touris|turism|\\bhotel\\b|\\bhoreca\\b|catering|\\bcafe\\b|\\bski\\b|\\bbar\\b|loisir|\\bsport|\\bdeporte|freizeit|bucatari", "I", "Hospitality & tourism",     "Low",           "",
  "immobili|inmobili|real estate|makler", "L", "Real estate",               "Low",           "",
  "conseil|consult|beratung|\\bavocat|\\bjuridi|\\bdroit\\b|\\blaw\\b|\\brecht\\b|anwalt|\\bnotari|\\bmarketing|communicati|publicit|\\brh\\b|ressources humaines|resurse umane|recursos humanos|human resources|recrut|\\bdesign|architektur|arquitect|arhitectur|architect|photograph|\\bart\\b|cultur|comunicaci|kommunikation|\\bmedia\\b|medien|\\bpresse|journalis|\\bredac|ingenieri|ingenieur|inginer|engineering", "M", "Professional/technical services", "Low",   "occupation",
  "nettoyag|limpieza|reinigung|\\bcleaning|proprete|curatenie|securit|seguridad|\\bsecurity|sicherheit|gardiennag|\\bpaza\\b|\\bcoiffur|\\bfriseur|\\bbeauty|\\bbeaute|esthetic", "N", "Support/personal services", "Low",           "",
  "artisan|handwerk|\\bmetier|\\bplomb|electric|\\belettric|menuis|charpent|\\bmacon\\b|installateur|schreiner|\\bmalerei", "F43", "Skilled trades (crafts)",   "Low",           "review",
  # --- generic buckets last (specific sectors above win first) --------------
  "servici|\\bservice|dienstleistung|tertiaire|prestari|\\bburo\\b|\\bbureau\\b|oficina|\\boffice\\b|secretari|atencion al cliente|kundenservice|call center|relation client", "N", "Generic services / office", "Low", "review"
)

# ---- Layer 2b: LLM-authored second pass (only for keys RULES left unmatched) -
# Ordered High -> Medium -> Low -> occupation so it cannot alter primary matches.
# Covers the long tail found in the first pass (incl. multilingual variants and
# common firm names). Flag "llm" marks these as second-pass, for validation.
RULES2 <- tibble::tribble(
  ~pattern,                                                                  ~nace, ~sector_en,                    ~tier,            ~flag,
  # High
  "mettalurg|\\bmetal\\b",                                                   "C24", "Basic metals",               "High",           "llm",
  "automovil|automovi|fahrzeugbau|\\bvaleo\\b",                              "C29", "Motor vehicles",             "High",           "llm",
  # Medium
  "\\bairbus\\b",                                                            "C30", "Aerospace manufacturing",    "Medium",         "llm;review",
  "elektronik|\\belectronic|electronica",                                   "C26", "Electronics",                "Medium",         "llm;review",
  "prelucrarea lemnului|\\bholz\\b|\\bbois\\b|scierie|cherestea|tamplari",   "C16", "Wood products",              "Medium",         "llm",
  "\\bdruck|imprimeri|imprenta|artes graficas|arte grafica|tipografi",      "C18", "Printing",                   "Medium",         "llm",
  "anlagenbau|\\bascensor",                                                  "C28", "Machinery & equipment",      "Medium",         "llm;review",
  "\\bpost\\b|\\bposta\\b|postal|livrai|livrare|livrator|livraison|repartidor|\\breparto\\b|\\bcariste\\b|stivuitor|manipulant|mozo de almacen|\\balmacen\\b|lagerist|auslieferung|schifffahrt|\\baviatie\\b|\\baviacion\\b|aerolinea|\\btaxi\\b|personenbeford|transporturi", "H", "Transport & logistics", "Medium", "llm;review",
  "dachdecker|baunebengewerbe|infrastruktur|infraestructura|instalatii",    "F",   "Construction",               "Medium",         "llm",
  "\\blandwirt|ganaderi|ganader|agrario|agronom|\\bpesca\\b|\\bpeche\\b|\\bfisch|pescuit|\\banimale\\b", "A", "Agriculture & forestry", "Medium", "llm",
  "bebidas|\\bcarnico\\b|\\bmolino\\b|\\bmoara\\b",                          "C10", "Food & beverages",           "Medium",         "llm",
  "versorgung|versorger|\\butilit",                                          "D35", "Energy (unspecified)",       "Medium",         "llm;review",
  "wasserversorg|gestion de residuos|gestionare deseuri|\\bdeseuri\\b|reciclaj", "E", "Water & waste",           "Medium",         "llm",
  "herstellung|verarbeitendes gewerbe|\\bmontage\\b|\\bfabrica|konsumguter", "C",   "Manufacturing (generic)",    "Medium",         "llm;review",
  # Low
  "\\bpharma\\b|pharmaci|pharmaco|\\bfarma\\b|farmaci|apotheke",             "C21", "Pharmaceuticals",            "Low",            "llm",
  "betreuung|krankenpfleg|\\bpfleg|physiotherap|\\bzahnarzt|rettungsdienst|\\bambulanc|\\bclinic|\\bdomicile\\b|auxiliaire de vie|garde d enfant|\\benfance\\b|cuidador|ingrijire|\\basistent|begleitung|\\bmfa\\b|\\bash\\b|dgasp|veterinar|tierpfleg|tierarzt|hebamme|psiholog|psicolog|psycholog|psychiatr|\\bsozial\\b|tehnica dentar", "Q", "Health & social care", "Low", "llm",
  "kindergarten|\\bkita\\b|ensenanz|docenc|docente|\\bformacion\\b|\\blehre\\b|ausbildung|\\bscoala|bibliotec", "P85", "Education & childcare", "Low", "llm",
  "polizei|policia|\\bpolitia\\b|regierung|\\bguvern|\\bbund\\b|bundeswehr|feuerwehr|\\bestado\\b|dgfip|\\bmapn\\b|aparare nationala|hacienda|bugetar|institutie publica|sector public|sectorul public|sistem public|sistemul public|off dienst|\\bpublique\\b|justici|justit|\\bmsa\\b|france travail|pole emploi|prefectur", "O84", "Public administration", "Low", "llm;broad",
  "vertrieb|vendedor|vendedora|\\bventa\\b|venta al por menor|venta minorista|vanzare|vanzatoare|vanzarea|dependient|\\bcasier\\b|caissier|\\bmagazin|supermarkt|minorista|buchhandel|\\bcarrefour|leclerc|\\blidl\\b|castorama|leroy merlin|\\bamazon\\b|autohaus|kaufland|auchan|mercadona|distribucion|distributie", "G47", "Retail trade", "Low", "llm",
  "finanzwesen|finanzbranche|finanzdienstleist|steuerberat|steuerbur|\\bsteuern\\b|\\baudit\\b|courtage|\\bbroker\\b|bausparkasse|krankenkasse|mutuelle|\\baxa\\b|\\bseguro\\b|\\bimpozit", "K", "Finance & insurance", "Low", "llm",
  "\\bti\\b|\\btic\\b|programaci|programator|desarrollo web|\\binternet\\b|\\bonline\\b|telefonia",  "J", "IT & telecom", "Low", "llm;occupation",
  "\\bgastro|gastgewerbe|\\bkoch\\b|\\bcuisine\\b|bucatar|ospatar|camarer|serveu|fitness|\\bocio\\b|divertisment|entretenimiento|unterhaltung|glucksspiel|parc attraction|\\bcinema\\b|\\bkino\\b|animation|evenement|\\bevent\\b|alojamien|ospitalitate|\\bbacker", "I", "Hospitality & tourism", "Low", "llm",
  "imobiliar|bienes raices|administrare imobile",                           "L",   "Real estate",                "Low",            "llm",
  "\\blegal\\b|abogac|\\bderecho\\b|\\bdrept\\b|jurist|rechtswesen|traduc|diseno|arqueolog|\\bphoto\\b|fotografi|blogger|content creator|comunicare|relatii publice|relations publiques|proiectare|cercetare|forschung|investigacion|\\bresearch\\b|recherche|wissenschaft|\\bstiinta|laboratoire|\\blabor\\b|asesoria|gestoria|consilier|audiovisual|\\bedition\\b|editorial|verlag|\\bkunst\\b|\\bkultur\\b|\\barta\\b|\\barts?\\b|medias|\\brrhh\\b|\\bpersonal\\b|werbung", "M", "Professional/technical services", "Low", "llm;occupation",
  "\\bmenage\\b|\\bentretien\\b|agent d entretien|blanchisserie|kosmetik|cosmetiqu|cosmetic|\\bestetica\\b|esthetique|\\bbeaute|\\bbeauty|tatouage|tattoo|funerair|funeral|bestattung|conserjeria|conserje|hausmeister|ingrijitor cladiri|\\bgarten\\b|\\bjardin|espaces verts|paysagi|kirche|biserica|\\bngo\\b|non profit|gemeinnutzig|tercer sector|sin animo de lucro|\\binterim\\b|haushaltshilfe|hauswirtschaft|domestico|empleada de hogar|mantenimiento|mentenanta|buroservice|telemarketing|teleoperador|call center|centre d appel", "N", "Support/personal services", "Low", "llm",
  # Occupation / no-sector (cannot assign exposure) -> Unclassifiable
  "\\bmanager\\b|\\bdirector\\b|\\btehnician\\b|\\btecnico\\b|\\btechniker\\b|freelanc|liber profesionist|\\bauxiliar\\b|\\bagent\\b|\\breferent\\b|analist|\\bcalidad\\b|\\bqualite\\b|angajat|salariat|mitarbeiter|\\badmin\\b|administrator|\\bbirou\\b|\\bbuero|secretar|burokauffrau|\\beinkauf\\b|kundendienst|kundenservice|relatii clienti|sachbearbeiter|dispecer|management|gestiune|\\bgestion\\b|\\bod\\b", NA, "Occupation (no sector)", "Unclassifiable", "llm;occupation"
)

classify_one <- function(key) {
  for (i in seq_len(nrow(RULES))) {
    if (str_detect(key, regex(RULES$pattern[i], ignore_case = TRUE))) {
      return(RULES[i, c("nace", "sector_en", "tier", "flag")])
    }
  }
  for (i in seq_len(nrow(RULES2))) {
    if (str_detect(key, regex(RULES2$pattern[i], ignore_case = TRUE))) {
      return(RULES2[i, c("nace", "sector_en", "tier", "flag")])
    }
  }
  tibble::tibble(nace = NA_character_, sector_en = NA_character_, tier = "", flag = "")
}

# ---- Read + build unique table ----------------------------------------------
read_q14 <- function(country, path) {
  raw <- read_csv(path, col_types = cols(.default = col_character()),
                  show_col_types = FALSE)
  raw %>%
    filter(.data$Status %in% STATUS_IP) %>%
    transmute(country = country, raw = str_trim(.data$Q14)) %>%
    filter(!is.na(.data$raw), .data$raw != "")
}

resp <- bind_rows(lapply(names(COUNTRY_FILES), function(c) {
  message("Reading ", COUNTRY_FILES[[c]])
  read_q14(c, COUNTRY_FILES[[c]])
}))

resp <- resp %>%
  mutate(
    normalized_key = normalize_key(.data$raw),
    # multi-answer: separator present with a second content token
    flag_multi = str_detect(.data$raw, "[/,;]|\\band\\b|\\bet\\b|\\bund\\b|\\by\\b|\\bsi\\b") &
      str_count(.data$normalized_key, "\\S+") >= 2,
    # primary token = text before first separator in the RAW string
    primary_raw = str_trim(str_replace(.data$raw, "\\s*[/,;].*$", "")),
    key_primary = normalize_key(.data$primary_raw)
  )

# Collapse to unique (country, normalized_key) with representative raw + freq.
unique_tbl <- resp %>%
  group_by(.data$country, .data$normalized_key) %>%
  summarise(
    raw        = names(sort(table(.data$raw), decreasing = TRUE))[1],
    freq       = n(),
    any_multi  = any(.data$flag_multi),
    key_class  = dplyr::first(.data$key_primary),  # classify on primary token
    .groups    = "drop"
  )

# Apply dictionary (on the primary-token key) + noise routing.
coded <- unique_tbl %>%
  mutate(is_noise = str_detect(.data$key_class, regex(NOISE_PATTERNS, ignore_case = TRUE)) |
           .data$key_class == "")

cls <- bind_rows(lapply(coded$key_class, classify_one))
coded <- bind_cols(coded, cls)

coded <- coded %>%
  mutate(
    tier = ifelse(.data$is_noise, "Unclassifiable", .data$tier),
    nace = ifelse(.data$is_noise, NA_character_, .data$nace),
    sector_en = ifelse(.data$is_noise, "Unclassifiable / non-sector", .data$sector_en),
    flags = {
      f <- .data$flag
      f <- ifelse(.data$is_noise, paste0(ifelse(nzchar(f), paste0(f, ";"), ""), "noise"), f)
      f <- ifelse(.data$any_multi, paste0(ifelse(nzchar(f), paste0(f, ";"), ""), "multi"), f)
      f
    }
  ) %>%
  rename(nace_div = "nace") %>%
  select(country, raw, normalized_key, freq, nace_div, sector_en, tier, flags) %>%
  arrange(.data$country, dplyr::desc(.data$freq), .data$normalized_key)

dir.create("data", showWarnings = FALSE)
write_csv(coded, "data/q14_sector_coded.csv")

# ---- Coverage report ---------------------------------------------------------
total_resp <- resp %>% count(country, name = "n_resp")
cov <- coded %>%
  mutate(matched = !(.data$tier %in% c("", "Unclassifiable"))) %>%
  group_by(.data$country) %>%
  summarise(
    n_keys        = n(),
    resp_covered  = sum(.data$freq[.data$matched]),
    resp_unmatched= sum(.data$freq[.data$tier == ""]),
    resp_unclass  = sum(.data$freq[.data$tier == "Unclassifiable"]),
    .groups = "drop"
  ) %>%
  left_join(total_resp, by = "country") %>%
  mutate(pct_covered = round(100 * .data$resp_covered / .data$n_resp, 1))

tier_tab <- coded %>%
  mutate(tier = ifelse(.data$tier == "", "(unmatched)", .data$tier)) %>%
  group_by(.data$country, .data$tier) %>%
  summarise(respondents = sum(.data$freq), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = "tier", values_from = "respondents", values_fill = 0)

cat("\n--- Coverage (respondents with a non-blank, non-Unclassifiable tier) ---\n")
print(cov)
cat("\n--- Respondents by tier ---\n")
print(tier_tab)
cat("\nWrote data/q14_sector_coded.csv (", nrow(coded), " unique keys)\n", sep = "")
