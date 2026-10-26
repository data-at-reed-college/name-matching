# Builds data/nicknames.rda from the nickname groups in the original
# fuzzy_name_match.R script. Run from the package root:
#   Rscript data-raw/nicknames.R

# only include gendered names
# ex: Alexander can include Al but not Alex (because of Alexandra)
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

nicknames <- data.frame(
  group = rep(seq_along(nickname_groups), lengths(nickname_groups)),
  name  = unlist(nickname_groups),
  stringsAsFactors = FALSE
)
nicknames <- tibble::as_tibble(nicknames)

save(nicknames, file = "data/nicknames.rda", compress = "xz")
