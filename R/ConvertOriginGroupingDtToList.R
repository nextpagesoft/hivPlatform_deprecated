ConvertOriginGroupingDtToList <- function(dtMap) {
  groupNames <- unique(dtMap$name)
  listMap <- lapply(groupNames, function(groupName) {
    list(
      name = groupName,
      origin = dtMap[name == groupName, sort(unique(origin))],
      migrant = dtMap[name == groupName, sort(unique(migrant))]
    )
  })
  return(listMap)
}
