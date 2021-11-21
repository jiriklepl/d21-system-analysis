#!/usr/bin/env Rscript
library('tidyverse')

votes <- read_delim(file='votes.csv', delim=';', col_names=FALSE)
preferences <- read_delim(file='preferences.csv', delim=',')

preferences <- preferences[c(-2),c(-1,-2,-3,-4, -ncol(preferences))]
preferences[preferences == '-'] <- NA
names(preferences) <- preferences[1,]
preferences <- preferences[-1,]

preferences[preferences == "Ne"] <- "-1"
preferences[preferences == "NE"] <- "-1"
preferences[preferences == "Ano"] <- "1"
preferences[is.na(preferences)] <- "0"
preferences <- sapply(preferences, as.integer)
preferences[is.na(preferences)] <- 0
preferences <- as_tibble(preferences)

cols <- ncol(preferences)
rows <- nrow(preferences)

agree <- sapply(
    1:cols,
    function(i) sapply(
        1:cols,
        function(j) 1 - sum(abs(preferences[,i] - preferences[,j])) / (2 * rows)))

rownames(agree) <- colnames(agree) <- colnames(preferences)

imagine <- function(each_votes, limit = .05, chairs = 200) {
    new_votes <- array(0, cols)
    names(new_votes) = colnames(agree)

    for (i in (1:cols)) {
        which <- order(agree[i,])[cols:(cols-each_votes + 1)]
        old_votes <- new_votes[which]
        new_votes[which] <- as.integer(votes[i, 2])
        new_votes[which] <-
            new_votes[which] + old_votes
    }

    new_votes <- new_votes / sum(each_votes * votes[,2])
    new_votes <- new_votes[new_votes > limit]

    new_votes <- chairs * new_votes / sum(new_votes)

    return (new_votes[order(-new_votes)])
}

imagine_ame <- function(each_votes, num_limit = 2, chairs = 200) {
    new_votes <- array(0, cols)
    names(new_votes) = colnames(agree)

    for (i in (1:cols)) {
        which <- order(agree[i,])[cols:(cols-each_votes + 1)]
        old_votes <- new_votes[which]
        new_votes[which] <- as.integer(votes[i, 2])
        new_votes[which] <-
            new_votes[which] + old_votes
    }

    new_votes <- new_votes / sum(each_votes * votes[,2])
    new_votes <- new_votes[order(-new_votes)]

    new_votes <- new_votes[1:min(cols, num_limit)]
    new_votes <- chairs * new_votes / sum(new_votes)

    return (new_votes)
}

sums <- rowSums(sapply(1:22, function(x) as.integer(votes[x,2]) * log(agree[x,]) / sum(log(agree[x,]))))
write("Order according to the disagreement measured in natural units:", stdout())
print(sums[order(sums)])

write("", stdout())

sums <- rowSums(sapply(1:22, function(x) as.integer(votes[x,2]) * agree[x,] / sum(agree[x,])))
write("Order according to the agreement measured in absolute units:", stdout())
print(sums[order(-sums)])


for (i in 2:5) {
    write("\n--------------\n", stdout())

    write(paste("Janecek's proposal in majority system with", i, "parties; based on casting", 2 * i, "votes:"), stdout())
    print(imagine_ame(2 * i, i, 200))

    write("", stdout())

    write(paste("Janecek's proposal in majority system with", i, "parties; based on casting", 2 * i + 1, "votes:"), stdout())
    print(imagine_ame(2 * i + 1, i, 200))

    write("", stdout())

    write("A system based on casting just one vote:", stdout())
    print(imagine_ame(1, i, 200))
}
