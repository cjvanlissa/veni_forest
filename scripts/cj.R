library(foreign)
imp_fun <- function(x){
  UseMethod("imp_fun", x)
}
imp_fun.data.frame <- function(x){
  data.frame(lapply(x, imp_fun))
}
imp_fun.default <- function(x){
  out <- x
  if(inherits(x, "numeric")){
    out[is.na(out)] <- mean(x[!is.na(out)])
  } else {
    out[is.na(out)] <- names(sort(table(out), decreasing = TRUE))[1]
    if(!class(out) == class(x)){
      tryCatch({
        out <- do.call(paste0("as.", class(x)[1]), list(out))
      }, error = function(e){
        stop("Could not impute missing values.")
      })
    }
  }
  out
}

df <- read.spss("c:/git_repositories/veni_forest/DADradar-27-07-2020-Van_Lissa_2.sav", to.data.frame = TRUE, use.value.labels = FALSE)
names(df) <- tolower(names(df))
names(df)[match("rir11aa19r", names(df))] <- "rir11aa19"

# Compare to requested scales
scalesrx <- read.csv("c:/git_repositories/veni_forest/selected_scales.csv", stringsAsFactors = FALSE)
scalesrx <- unlist(sapply(scalesrx$item.names, function(x){strsplit(x, " ")[[1]]}, USE.NAMES = FALSE))
# Only parent sex and age missing, but we have other variables for this
#scalesrx[!scalesrx %in% names(df)]

# Check what these are
write.table(names(df)[!names(df) %in% scalesrx], "clipboard", sep = "\n", row.names = F, col.names = F)

# Recode missings
is_mis <- df == 888|df == 999
df[is_mis] <- NA
is_mis <- sapply(df, trimws)
is_mis <- is_mis == ""
df[is_mis] <- NA

#which(colSums(is_mis, na.rm = TRUE) > 0)

# Retain only relevant variables
dropthese <- c("id_gezin", "id_gezin_jonglabel", "idp_tar", "idp_moe", "radar_lb",
               "moeder_wisseling", "sib_wisseling", "vader_wisseling", "age.difference.target.between.w6.and.w8",
               "age.difference.target.between.w8.and.w9", "idp_bv11", "idp_bv12",
               "bg11aa06b", "bg11aa07b", "bg11aa08b",
               "bg11mm07a", "bg11mm07b",
               "bv_sex12", "idp_bv13", "bv_sex13", "idp_bv14", "bv_sex14", "idp_bv21",
               "bv_sex21", "idp_bv22", "bv_sex22", "idp_bv23", "bv_sex23", "idp_bv24",
               "bv_sex24", "idp_bv31", "bv_sex31", "idp_bv32", "bv_sex32", "idp_bv33",
               "bv_sex33", "idp_bv34", "bv_sex34", "idp_bv41", "bv_sex41", "idp_bv42",
               "bv_sex42", "idp_bv43", "bv_sex43", "idp_bv44", "bv_sex44", "idp_bv51",
               "bv_sex51", "idp_bv52", "bv_sex52", "idp_bv53", "bv_sex53", "idp_bv54",
               "bv_sex54", "idp_bv61", "bv_sex61", "sib_sex21", "sib_sex31",
               "sib_sex41", "sib_sex51", "sib_sex61", "par_sex_71", "rpp", "brpmoe_low",
               "brpvad_low", "parti_aa_w11", "parti_aa_w21", "parti_aa_w31",
               "parti_aa_w41", "parti_aa_w51", "parti_aa_w61", "parti_aa_w71",
               "parti_bb_w11", "parti_bb_w21", "parti_bb_w31", "parti_bb_w41",
               "parti_bb_w51", "parti_bb_w61", "parti_ss_w11", "parti_ss_w21",
               "parti_ss_w31", "parti_ss_w41", "parti_ss_w51", "parti_ss_w61",
               "parti_mm_w11", "parti_mm_w21", "parti_mm_w31", "parti_mm_w41",
               "parti_mm_w51", "parti_mm_w61", "parti_vv_w11", "parti_vv_w21",
               "parti_vv_w31", "parti_vv_w41", "parti_vv_w51", "parti_vv_w61",
               "parti_ii_w71", "leeftijd_target_21", "leeftijd_target_31", "leeftijd_target_41",
               "leeftijd_target_51", "leeftijd_target_61", "leeftijd_target_71",
               "leeftijd_target_81", "leeftijd_target_91", "leeftijd_bestevriend_21",
               "leeftijd_bestevriend_31", "leeftijd_bestevriend_41", "leeftijd_bestevriend_51",
               "leeftijd_bestevriend_61", "leeftijd_moeder_21", "leeftijd_moeder_31",
               "leeftijd_moeder_41", "leeftijd_moeder_51", "leeftijd_moeder_61",
               "leeftijd_sibling_21", "leeftijd_sibling_31", "leeftijd_sibling_41",
               "leeftijd_sibling_51", "leeftijd_sibling_61", "leeftijd_sibling_81",
               "leeftijd_sibling_91", "leeftijd_vader_21", "leeftijd_vader_31",
               "leeftijd_vader_41", "leeftijd_vader_51", "leeftijd_vader_61",
               "leeftijd_partner_71", "leeftijd_partner_81", "leeftijd_partner_91",
               "br11amtot", "br11avtot", "bf11aaext", "bf11aavri", "bf11aazor",
               "bf11aaemo", "bf11aaope", "bb11aabis", "bb11aabar", "bb11aabad",
               "bb11aabaf", "cb11madel", "cb11maagr", "cb11maext", "cb11vadel",
               "cb11vaagr", "cb11vaext", "ca11aoca", "cr11abce", "cr11abps",
               "cr11abco", "cr11abwi", "cr11avce", "cr11avps", "cr11avco", "cr11avwi",
               "cr11amce", "cr11amps", "cr11amco", "cr11amwi", "dh11mmtot",
               "dh11vvtot",
               "dm11aa17r", "dm11aa19r",
               "dm11aabli", "dm11aaboo", "dm11aaban", "dm11aaver",
               "dm11aasch", "dm11aamoe", "dm11aages", "zd11aavar", "zd11aafrq",
               "zd11aagew", "zd11aaver", "zd11aaord", "bl11aadel", "bl11aaint",
               "ic11amtot", "ic11avtot", "ic11matot", "ic11vatot", "ir11aafan",
               "ir11aaped", "ir11aaemp", "ir11aapet", "le11aoge", "le11aoop",
               "le11aoir", "le11aokr", "le11aook", "le11aotot", "nr11absup",
               "nr11abneg", "nr11abpow", "nr11amsup", "nr11amneg", "nr11ampow",
               "nr11avsup", "nr11avneg", "nr11avpow", "nr11masup", "nr11maneg",
               "nr11mapow", "nr11vasup", "nr11vaneg", "nr11vapow", "pp11ampk",
               "pp11amad", "pp11amps", "pp11ampc", "pp11avpk", "pp11avad", "pp11avps",
               "pp11avpc", "pp11mapk", "pp11maad", "pp11maps", "pp11mapc", "pp11vapk",
               "pp11vaad", "pp11vaps", "pp11vapc", "pc11amtot", "pc11avtot",
               "pd11maco", "pm11mapro", "pm11vapro", "pm11ampro", "pm11avpro",
               "pr11aapat", "pr11aapap", "pr11aapar", "pr11aarat", "pr11aarap",
               "pr11aarar", "pr11aavat", "pr11aavpa", "pr11aavra", "pb11aatot",
               "ra11aadym", "ra11aaanh", "ra11aanes", "ra11aasco", "ra11aatot",
               "an11aasop", "an11aasch", "an11aasoc", "an11aagan", "an11aasan",
               "an11aatot", "sc11aasel", "su11aahd", "ys11aadel", "ys11aaagr",
               "ys11aaext", "ss11aa02z", "ss11aa02a", "ss11aa02b", "ss11aa02c",
               "ss11aa02d", "ss11aa02e", "ss11aa02f", "ss11aa02g", "ss11aa02h",
               "su11aa12", "su11aa13", "su11aa13a", "bg41aa14b3", "bg11mm04a",
               "zd11aa04a", "zd11aa05a", "zd11aa08a", "zd11aa24a", "zd11aa01d",
               "zd11aa02d", "zd11aa03d", "zd11aa04d", "zd11aa05d", "zd11aa06d",
               "zd11aa07d", "zd11aa08d", "zd11aa09d", "zd11aa10d", "zd11aa11d",
               "zd11aa12d", "zd11aa13d", "zd11aa14d", "zd11aa15d", "zd11aa16d",
               "zd11aa17d", "zd11aa18d", "zd11aa19d", "zd11aa20d", "zd11aa21d",
               "zd11aa22d", "zd11aa23d", "zd11aa24d", "zd11aa25d", "zd11aa26d",
               "zd11aa27d", "zd11aa28d", "zd11aa29d", "zd11aa30d",
               "ss11aa01z", "ss11aa01a", "ss11aa01b", "ss11aa01c", "ss11aa01d",
               "ss11aa01e", "ss11aa01f", "ss11aa01g", "ss11aa01h", "ss11aa03z",
               "ss11aa04z", "ss11aa05z"
)

# Check data
desc <- descriptives(df)

# Drop variables with >90% missing
df[desc$name[desc$missing > .9]] <- NULL


# Remove free text variables
desc <- descriptives(df)
tmp <- desc[desc$type %in% c("character", "factor"), ]
if(nrow(tmp) > 0){
  df_txt <- df[tmp$name]
  df_txt <- sapply(df_txt, trimws)
  df_txt[df_txt == ""] <- NA
  df_txt <- data.frame(df_txt)
  maxlength <- t(sapply(df_txt, function(x){
    txt <- as.character(x)
    ncr <- nchar(txt, keepNA = TRUE)
    c(max(ncr, na.rm = TRUE), txt[which.max(ncr)])
  }))
  desc_txt <- descriptives(df_txt)
}

vars <- names(df)
dvs <- grep("^r?de\\d{2}aa\\d+$", vars, value = TRUE)
ivs <- vars[!vars %in% dvs]
ivs <- ivs[!grepl("^[a-z]{2,3}[2-9]\\d", ivs)]
ivs <- ivs[!ivs %in% dropthese]

grep("^\\w{2}\\d{2}[a-z]+$", ivs, value = T)

ivs[!ivs %in% scalesrx]

ivs <- ivs[!(grepl("^[a-z]+\\d{2}", ivs) & !grepl("^[a-z]+11", ivs))]
ivs <- ivs[!(grepl("\\d{2}$", ivs) & !grepl("11$", ivs))]
ivs <- ivs[!ivs %in% c()]
#ivs[!(grepl("^[a-z]+11", ivs)|grepl("11$", ivs))]
#ivs <- ivs[grepl("^[a-z]+11", ivs) | (!grepl("^[a-z]+11", ivs) & grepl("11$", ivs))]
df <- df[, c(dvs, ivs)]
df[grep("^ss11aa0\\d[a-z]$", names(df))] <- NULL

# Pseudonymize
set.seed(436)
df <- df[sample(1:nrow(df)), ]



table(sapply(df, class))




#!
tmp <- synthetic(df, model_expression = NULL, predict_expression = sample(y, 497, replace = TRUE), missingness_expression = imp_fun(x = data))
desc[c(214, 215), ]

library(tidyLPA)
a[]
tidyLPA::single_imputation()
