r_perim = 2
canPlace <- function(lots, company, r, c) {
  #check if there is a row in lots that matches x and y
  companyInLot = lots[r, c]
  if (is.na(companyInLot) | companyInLot != '')
    return (F)
  #check all lots in perimeter of this x and y and get companies and see if they conflict
  for (row in seq(max(1, r - r_perim), min(nrow(lots), r + r_perim))) {
    for (column in seq(max(1, c - r_perim), min(ncol(lots), c + r_perim))) {
      if (row == r & column == c)
        next
      companyInLot = lots[row, column]
      if (!(is.na(company) |
            is.na(companyInLot) |
            company == '' | companyInLot == ''))
        if (companies[companies$company == company, ]$sector == companies[companies$company ==
                                                                          companyInLot, ]$sector)
          return (F)
    }
  }
  return (T)
  
}

#lotsInput can already be partially filled.
#don't forget to only input those companies that haven't been assigned to a lot!
randomlotcompany <- function(companies, lotsInput) {
  lots = data.frame(lotsInput,stringsAsFactors = F)
  unassignedCompanies = data.frame(companies,stringsAsFactors = F)
  emptyLots = data.frame(stringsAsFactors = F)
  for (c in 1:ncol(lots))
    for (r in 1:nrow(lots))
      if (!is.na(lots[r, c]) & lots[r, c] == '')
        emptyLots = rbind(emptyLots, c(r, c))
  # cat(
  #   "empty lots:",
  #   nrow(emptyLots),
  #   " unassigned companies:",
  #   nrow(unassignedCompanies),
  #   '\n'
  # )
  while (nrow(emptyLots) > 0 & nrow(unassignedCompanies) > 0) {
    lot = sample(1:nrow(emptyLots), 1)
    r = emptyLots[lot, 1]
    c = emptyLots[lot, 2]
    emptyLots = emptyLots[-lot,]
    company = sample(1:nrow(unassignedCompanies), 1)
    ntry = 0
    while (!canPlace(lots, unassignedCompanies[company, ]$company, r, c)) {
      company = sample(1:nrow(unassignedCompanies), 1)
      ntry = ntry + 1
        if (ntry >= 100) {
          cat("randomlotcompany failed! (ntry>=100)","\n")
          return (list(F,lots,nrow(unassignedCompanies)))
        }
    }
    lots[r, c] = unassignedCompanies[company, ]$company
    unassignedCompanies = unassignedCompanies[-company,]
    # cat(
    #   "empty lots:",
    #   nrow(emptyLots),
    #   " unassigned companies:",
    #   nrow(unassignedCompanies),
    #   '\n'
    # )
  }
  cat("1")
  if (nrow(unassignedCompanies) > 0) {
    cat("2")
    cat("randomlotcompany failed! (unassignedCompanies > 0)","\n")
    return (list(F,lots,nrow(unassignedCompanies)))
  }
  else{
    cat("3")
    return (list(T,lots,nrow(unassignedCompanies)))
  }
}

biggestsectorsfirst <- function(companies, lotsInput) {
  lots = data.frame(lotsInput,stringsAsFactors = F)
  
  sectors = companies %>%
    group_by(sector) %>%
    summarise(n = n())
  
  unassignedCompanies = companies %>%
    left_join(sectors, by = 'sector')   %>%
    arrange(desc(n))
  
  emptyLots = data.frame(stringsAsFactors = F)
  for (c in 1:ncol(lots))
    for (r in 1:nrow(lots))
      if (!is.na(lots[r, c]) & lots[r, c] == '')
        emptyLots = rbind(emptyLots, c(r, c))
  # cat(
  #   "empty lots:",
  #   nrow(emptyLots),
  #   " unassigned companies:",
  #   nrow(unassignedCompanies),
  #   '\n'
  # )
  for (i in 1:nrow(unassignedCompanies)) {
    assigned = F
    for (n in sample(1:nrow(emptyLots))) {
      r = emptyLots[n, 1]
      c = emptyLots[n, 2]
      if (canPlace(lots, unassignedCompanies[i,]$company, r, c)) {
        lots[r, c] = unassignedCompanies[i,]$company
        emptyLots = emptyLots[-n,]
        assigned = T
        # cat(
        #   "empty lots:",
        #   nrow(emptyLots),
        #   " unassigned companies:",
        #   nrow(unassignedCompanies) - i,
        #   '\n'
        # )
        break
        
      }
    }
    if (!assigned) {
      cat('biggestsectorsfirst failed! (lots exhausted)',"\n")
      return (list(F,lots,nrow(unassignedCompanies)+1-i))
    }
  }
  return (list(T,lots,0))
}