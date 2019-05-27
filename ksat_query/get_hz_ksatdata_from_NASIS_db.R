## Modified from soilDB function: get_hz_data_from_NASIS_db.R
# 
get_hz_ksatdata_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT peiid, phiid, upedonid as pedon_id,
  hzname, dspcomplayerid as genhz, hzdept, hzdepb, taxonname,
  sathydcondstd, sathydcondmean, sathydcondmethod, repnum, sathydcondrepmean, sathydcondrepstd, sathydcondclass, notes, ksatreadingnum, sathydcondmeasured, steadystateflag, pedonksatsummaryiid, phksatamoozemeteriid, peksatamoozedataiid, boreholedepth, boreholewaterlevelinit, boreholewaterlevelfinal
  
  FROM 
  pedon_View_1 p 
  INNER JOIN phorizon_View_1 ph ON ph.peiidref = p.peiid 
  LEFT OUTER JOIN phsample_View_1 phs ON phs.phiidref = ph.phiid
  LEFT OUTER JOIN phksatsummary_View_1 phk ON phk.phiidref = ph.phiid
  LEFT OUTER JOIN phksatamoozemeter_View_1 phka ON phka.phksatsumiidref = phk.pedonksatsummaryiid
  LEFT OUTER JOIN phksatamoozedata_View_1 phkad ON phkad.peksatamoozeiidref = phka.phksatamoozemeteriid
  
  ORDER BY p.upedonid, ph.hzdept ASC;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # test for duplicate horizons:
  #  bugs in our queries
  #   multiple lab samples / genetic horizon
  hz.tab <- table(d$phiid)
  dupe.hz <- which(hz.tab > 1)
  dupe.hz.phiid <- names(hz.tab[dupe.hz])
  dupe.hz.pedon.ids <- d$pedon_id[d$phiid %in% dupe.hz.phiid]
  
  if (length(dupe.hz) > 0) {
    message(paste('NOTICE: multiple `labsampnum` values / horizons; see pedon IDs:\n', paste(unique(dupe.hz.pedon.ids), collapse=','), sep=''))
  }
  
  # close connection
  RODBC::odbcClose(channel)
  
  # done
  return(d)
}

