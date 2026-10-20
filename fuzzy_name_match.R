# Fuzzy matching of names in one table (df1) to names in a second table (df2)
#
# Needs only two data frames of names:
#   df1 : the names to be matched       columns: id (unique), first, last
#   df2 : the names matched against     columns: id (unique), first, last
# Each row of df1 is matched to at most one row of df2. Other columns are carried along.
# Steps, in order (a row matched in one step is not considered again):
#   exact name -> token set -> Jaro-Winkler typos -> Soundex -> nicknames -> first word only

library(tidyverse)
library(fuzzyjoin)

# ======================================================================
# Read in the two files to be matched (placeholder: change the paths)
# Each needs columns id (unique), first, last; rename yours here if they differ
# ======================================================================
df1 <- read_csv("path/to/file1.csv", col_types = cols(.default = col_character()))
df2 <- read_csv("path/to/file2.csv", col_types = cols(.default = col_character()))
# df1 <- rename(df1, first = firstName, last = lastName)   # example: rename to first / last

# ======================================================================
# Normalize names (before any matching) in both files
# all lower caseuppercase, strip accents/punctuation, hyphens become spaces, 
# drop suffixes (JR) and titles (MRS)
# ======================================================================
norm_name <- function(x) {
  suffix <- "(^| )(JR|SR|II|III|IV|DR|MR|MRS|MS)( |$)"
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_replace_all("-", " ") |>                # hyphens act as spaces
    str_remove_all("[^A-Z0-9 ]") |>             # drop punctuation (O'BRIEN -> OBRIEN)
    str_replace_all(suffix, " ") |>             # remove suffixes
    str_replace_all(suffix, " ") |>             # run twice for adjacent tokens
    str_squish() |>                             # removes extra white space
    na_if("")
}

# ======================================================================
# Nickname table (used by the nickname stage)
# ======================================================================
# only include gendered names
# ex: Alexander can include Al but not Alex (because of Alexandra)
# this is list built by Claude, it could probably be separated by strictness
nickname_groups <- list(
  c("ROBERT", "BOB", "BOBBY", "ROB", "ROBBIE"), c("WILLIAM", "BILL", "BILLY", "WILL", "WILLIE"),
  c("RICHARD", "RICK", "RICKY", "DICK", "RICH"), c("JAMES", "JIM", "JIMMY"),
  c("JOHN", "JOHNNY"), c("JOSEPH", "JOE", "JOEY"), c("THOMAS", "TOM", "TOMMY"),
  c("CHARLES", "CHARLIE", "CHUCK"), c("MICHAEL", "MIKE"), c("DANIEL", "DAN", "DANNY"),
  c("DAVID", "DAVE"), c("EDWARD", "ED", "EDDIE", "TED", "NED"), c("ANTHONY", "TONY"),
  c("STEPHEN", "STEVEN", "STEVE"), c("ANDREW", "ANDY", "DREW"), c("CHRISTOPHER", "CHRIS"),
  c("MATTHEW", "MATT"), c("NICHOLAS", "NICK"), c("BENJAMIN", "BEN"),
  c("ALEXANDER", "AL"), c("TIMOTHY", "TIM", "TIMMY"),
  c("DONALD", "DON", "DONNIE"), c("RONALD", "RON", "RONNIE"), c("KENNETH", "KEN", "KENNY"),
  c("GREGORY", "GREG"), c("JEFFREY", "JEFF"), c("PATRICK", "PAT"), c("LAWRENCE", "LARRY"),
  c("FREDERICK", "FRED", "FREDDIE"), c("LEONARD", "LEN", "LENNY"), c("PETER", "PETE"), c("CARL", "KARL"),
  c("ELIZABETH", "LIZ", "BETH", "LIZZIE"), c("MARGARET", "PEGGY", "MAGGIE", "MEG"),
  c("KATHERINE", "CATHERINE", "KATHRYN", "KATE", "KATHY", "CATHY", "CATE", "CATHRYN", "CATH", "KATH"),
  c("JENNIFER", "JEN", "JENNY", "JENN"), c("PATRICIA", "PATTY", "TRICIA"), c("BARBARA", "BARB"),
  c("SUSAN", "SUE", "SUSIE", "SUZANNE"), c("DEBORAH", "DEBRA", "DEB", "DEBBIE"), c("REBECCA", "BECKY", "BECCA"),
  c("CHRISTINA", "CHRISSY", "TINA"), c("VICTORIA", "VICKY", "VICKI"),
  c("JESSICA", "JESS", "JESSIE"), c("ABIGAIL", "ABBY"), c("DOROTHY", "DOT", "DOTTIE"), c("JACQUELINE", "JACKIE"),
  c("THERESA", "TERESA", "TERRI", "TERRY"), c("CARRIE", "KERRI", "KARI"), c("JULIA", "JULIE"), 
  c("STEPHANIE", "STEPH"), c("AMANDA", "MANDY"), c("JONATHAN", "JON", "JONATHON"), 
  c("PHILIP", "PHILLIP", "PHIL"), c("RAYMOND", "RAY"), c("ZACHARY", "ZACH", "ZAC", "ZACK"),
  c("PAMELA", "PAM"), c("NICHOLAS", "NIC", "NICK"), c("NATHANIEL", "NATHAN", "NATE"), 
  c("NATALIE", "NAT", "NATALIA"), c("GABRIEL", "GABE"), c("GABRIELLE", "GABRIELA", "GABBY"), 
  c("JOSHUA", "JOSH"), c("KIMBERLY", "KIM"), c("BRADLEY", "BRAD"), c("MARCUS", "MARC"), 
  c("MAXWELL", "MAXIMILIAN", "MAX"), c("MITCHELL", "MITCH"), c("RANDALL",  "RANDY"), c("STANLEY", "STAN"), 
  c("VINCENT", "VINCE", "VINNY"), c("WALTER", "WALT"), c("TIFFANY", "TIFF"), c("MADELINE", "MADELEINE", "MADDIE"), 
  c("ISABELLE", "ISABEL"), c("KRISTEN", "KRISTIN", "KRISTINE", "KRIS"),
  c("FRANCES", "FRAN"), c("ANGELA", "ANGIE")
)
# this makes a matrix of matches
nickname_pairs <- map_dfr(nickname_groups, \(g) expand_grid(a = g, b = g) |> filter(a != b)) |> distinct()

# ======================================================================
# Exact name match
# Accepted only if exactly one reference row has the name. Names shared by several
# reference rows are not resolved (no way to choose yet) and stay unmatched.
# ======================================================================
d <- df1 |>
  transmute(id = as.character(id), first_key = norm_name(first), last_key = norm_name(last))
v <- df2 |>
  transmute(ref_id = as.character(id), v_first = norm_name(first), v_last = norm_name(last)) |>
  filter(!is.na(v_first), !is.na(v_last))

exact <- d |>
  inner_join(count(v, v_first, v_last, name = "n_ref"), by = c(first_key = "v_first", last_key = "v_last"))

matches <- exact |>
  filter(n_ref == 1) |>
  inner_join(v, by = c(first_key = "v_first", last_key = "v_last")) |>
  transmute(id, ref_id, match_step = "exact_name")

# names with no exact reference name: the pool for the fuzzy steps (shrinks after every step)
pool <- d |> filter(!is.na(first_key), !is.na(last_key), !id %in% exact$id)

# ======================================================================
# Token-set match: same words in first + last name combined, in any order or split
# (covers swapped names, hyphen/space differences, a name moved between first/last)
# Each fuzzy step builds `cands` (donor name x candidate ref row), then accepts only donor
# names with exactly one candidate ref row.
# ======================================================================
token_key <- \(first, last) vapply(str_split(paste(first, last), " "), \(t) paste(sort(t), collapse = " "), character(1))

cands <- distinct(pool, first_key, last_key) |>
  mutate(tok = token_key(first_key, last_key)) |>
  inner_join(mutate(v, tok = token_key(v_first, v_last)), by = "tok", relationship = "many-to-many") |>
  filter(first_key != v_first | last_key != v_last) |>
  transmute(first_key, last_key, ref_id, match_step = "token_set")

accepted <- cands |>
  distinct() |>
  group_by(first_key, last_key) |> filter(n_distinct(ref_id) == 1) |> ungroup() |>
  inner_join(pool, by = c("first_key", "last_key"), relationship = "many-to-many") |>
  distinct(id, .keep_all = TRUE) |>
  select(id, ref_id, match_step)
matches <- bind_rows(matches, accepted)
pool    <- filter(pool, !id %in% accepted$id)

# ======================================================================
# Jaro-Winkler typos: first name similar + last name exact, or first name exact + last name similar
# Accepted only if jw_distance < jw_accept
# ======================================================================
jw_cutoff <- 0.9    # Jaro-Winkler similarity needed for a typo to be a candidate
jw_accept <- 0.05   # a typo candidate is accepted only if its JW distance (1 - similarity) is below this

dn <- distinct(pool, first_key, last_key)

# Pairs of similar first names (JW similarity >= jw_cutoff, same first letter, both >= 4 characters)
d_first <- tibble(d_name = unique(dn$first_key)) |> filter(nchar(d_name) >= 4)
v_first_names <- tibble(v_name = unique(v$v_first)) |> filter(nchar(v_name) >= 4)
first_pairs <- map_dfr(unique(str_sub(d_first$d_name, 1, 1)), \(b) {
  stringdist_inner_join(
    filter(d_first, str_sub(d_name, 1, 1) == b), filter(v_first_names, str_sub(v_name, 1, 1) == b),
    by = c(d_name = "v_name"), method = "jw", p = 0.1,
    max_dist = 1 - jw_cutoff + 1e-9, distance_col = "dist"   # stringdist JW distance = 1 - similarity
  )
}) |>
  filter(dist > 0) |>
  select(d_name, v_name, jw_distance = dist)

# Same for last names (both >= 5 characters)
d_last <- tibble(d_name = unique(dn$last_key)) |> filter(nchar(d_name) >= 5)
v_last_names <- tibble(v_name = unique(v$v_last)) |> filter(nchar(v_name) >= 5)
last_pairs <- map_dfr(unique(str_sub(d_last$d_name, 1, 1)), \(b) {
  stringdist_inner_join(
    filter(d_last, str_sub(d_name, 1, 1) == b), filter(v_last_names, str_sub(v_name, 1, 1) == b),
    by = c(d_name = "v_name"), method = "jw", p = 0.1,
    max_dist = 1 - jw_cutoff + 1e-9, distance_col = "dist"
  )
}) |>
  filter(dist > 0) |>
  select(d_name, v_name, jw_distance = dist)

cands <- bind_rows(
  dn |>
    inner_join(first_pairs, by = c(first_key = "d_name"), relationship = "many-to-many") |>
    inner_join(v, by = c(v_name = "v_first", last_key = "v_last"), relationship = "many-to-many") |>
    transmute(first_key, last_key, ref_id, match_step = "fuzzy_first_typo", jw_distance),
  dn |>
    inner_join(last_pairs, by = c(last_key = "d_name"), relationship = "many-to-many") |>
    inner_join(v, by = c(v_name = "v_last", first_key = "v_first"), relationship = "many-to-many") |>
    transmute(first_key, last_key, ref_id, match_step = "fuzzy_last_typo", jw_distance)
)

accepted <- cands |>
  group_by(first_key, last_key) |> filter(n_distinct(ref_id) == 1) |> ungroup() |>
  filter(jw_distance < jw_accept) |>
  inner_join(pool, by = c("first_key", "last_key"), relationship = "many-to-many") |>
  arrange(jw_distance) |> distinct(id, .keep_all = TRUE) |>
  select(id, ref_id, match_step)
matches <- bind_rows(matches, accepted)
pool    <- filter(pool, !id %in% accepted$id)

# ======================================================================
# Soundex: first AND last name sound alike (same Soundex code), e.g. SMITH / SMYTH, CATHRYN / KATHRYN
# Spaces are removed before coding, so multi-word names are coded as one word.
# ======================================================================
soundex <- \(x) stringdist::phonetic(str_remove_all(x, " "), method = "soundex")

cands <- distinct(pool, first_key, last_key) |>
  mutate(sx_first = soundex(first_key), sx_last = soundex(last_key)) |>
  inner_join(mutate(v, sx_first = soundex(v_first), sx_last = soundex(v_last)),
             by = c("sx_first", "sx_last"), relationship = "many-to-many") |>
  transmute(first_key, last_key, ref_id, match_step = "soundex")

accepted <- cands |>
  distinct() |>
  group_by(first_key, last_key) |> filter(n_distinct(ref_id) == 1) |> ungroup() |>
  inner_join(pool, by = c("first_key", "last_key"), relationship = "many-to-many") |>
  distinct(id, .keep_all = TRUE) |>
  select(id, ref_id, match_step)
matches <- bind_rows(matches, accepted)
pool    <- filter(pool, !id %in% accepted$id)

# ======================================================================
# Nicknames: last name exact, first name is a known nickname/formal pair
# ======================================================================
cands <- distinct(pool, first_key, last_key) |>
  inner_join(nickname_pairs, by = c(first_key = "a"), relationship = "many-to-many") |>
  inner_join(v, by = c(b = "v_first", last_key = "v_last"), relationship = "many-to-many") |>
  transmute(first_key, last_key, ref_id, match_step = "fuzzy_nickname")

accepted <- cands |>
  distinct() |>
  group_by(first_key, last_key) |> filter(n_distinct(ref_id) == 1) |> ungroup() |>
  inner_join(pool, by = c("first_key", "last_key"), relationship = "many-to-many") |>
  distinct(id, .keep_all = TRUE) |>
  select(id, ref_id, match_step)
matches <- bind_rows(matches, accepted)
pool    <- filter(pool, !id %in% accepted$id)

# ======================================================================
# First word only: last name exact, first WORD of the first name matches
# (drops middle names / initials stuck in the first-name field on either side,
#  e.g. LISA E vs LISA, ANN ELOISE vs ANN, LAUST vs LAUST JOHAN)
# ======================================================================
cands <- distinct(pool, first_key, last_key) |>
  mutate(first_w = str_extract(first_key, "^[^ ]+")) |>
  inner_join(mutate(v, first_w = str_extract(v_first, "^[^ ]+")),
             by = c("first_w", "last_key" = "v_last"), relationship = "many-to-many") |>
  filter(first_key != v_first) |>
  transmute(first_key, last_key, ref_id, match_step = "first_word")

accepted <- cands |>
  distinct() |>
  group_by(first_key, last_key) |> filter(n_distinct(ref_id) == 1) |> ungroup() |>
  inner_join(pool, by = c("first_key", "last_key"), relationship = "many-to-many") |>
  distinct(id, .keep_all = TRUE) |>
  select(id, ref_id, match_step)
matches <- bind_rows(matches, accepted)
pool    <- filter(pool, !id %in% accepted$id)

# ======================================================================
# Results: original columns only (clashing df2 column names get "_ref")
# ======================================================================
matched <- matches |>
  inner_join(mutate(df1, id = as.character(id)), by = "id") |>
  inner_join(mutate(df2, id = as.character(id)) |> rename(ref_id = id), by = "ref_id", suffix = c("", "_ref")) |>
  relocate(match_step, .after = last_col())

unmatched <- filter(mutate(df1, id = as.character(id)), !id %in% matches$id)

count(matches, match_step)   # matches contributed by each step
