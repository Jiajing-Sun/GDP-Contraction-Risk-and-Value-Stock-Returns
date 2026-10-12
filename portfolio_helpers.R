attach_outcomes <-
function (S, q, cols) 
{
    for (c in cols) S[[c]] <- q[[c]][match(qkey(S$quarter) + 
        1, qkey(q$quarter))]
    S
}
get32 <-
function (file, weight = "Value") 
{
    m <- read_block(file, weight)
    stopifnot(ncol(m) == 33)
    names(m)[2:33] <- paste0("S", rep(1:2, each = 16), "B", rep(rep(1:4, 
        each = 4), 2), "P", rep(1:4, 8))
    q <- quarterly(m, names(m)[2:33])
    for (s in 1:2) {
        for (p in c(1, 4)) q[[paste0("S", s, "_GV_OP", p)]] <- q[[paste0("S", 
            s, "B1P", p)]] - q[[paste0("S", s, "B4P", p)]]
        q[[paste0("S", s, "_contrast")]] <- q[[paste0("S", s, 
            "_GV_OP1")]] - q[[paste0("S", s, "_GV_OP4")]]
    }
    q
}
read_block <-
function (file, weight = "Value") 
{
    lines <- readLines(unz(file, unzip(file, list = TRUE)$Name[1]), 
        warn = FALSE)
    a <- grep(paste0("Average ", weight, " Weighted Returns -- Monthly"), 
        lines, fixed = TRUE)
    stopifnot(length(a) == 1)
    start <- a + 2
    while (!grepl("^\\s*[0-9]{6}\\s*,", lines[start])) start <- start + 
        1
    end <- start
    while (end < length(lines) && grepl("^\\s*[0-9]{6}\\s*,", 
        lines[end + 1])) end <- end + 1
    d <- read.csv(text = paste(lines[(start - 1):end], collapse = "\n"), 
        check.names = FALSE)
    names(d)[1] <- "time"
    d$time <- trimws(as.character(d$time))
    for (j in 2:ncol(d)) {
        d[[j]] <- as.numeric(d[[j]])
        d[[j]][d[[j]] %in% c(-99.989999999999995, -999)] <- NA_real_
    }
    d
}
