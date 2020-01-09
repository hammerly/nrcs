gtlbrts <- function (SS = TRUE) 
{
  sampledepthbottom <- NULL
  sampledepthtop <- NULL
  phiidref <- NULL
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  q <- "SELECT phiidref, seqnum, sampledepthtop, sampledepthbottom, sampleid, datacollector, claytotmeasured, claycarbmeasured, silttotmeasured, siltfinemeasured, siltcomeasured, sandtotmeasured, sandtotmethod, sandvcmeasured, sandcomeasured, sandmedmeasured, sandfinemeasured, sandvfmeasured, sandvfmethod, textureclfieldlab, fiberrubbedpct, fiberunrubbedpct, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, phdeltah2o2, liquidlimitmeasured, plasticlimitmeasured, pi, atterbergsampcond, cole, esttotpotacidityetpa, camgmeh2, potassiummeh2, camgsatpaste, extractaciditykcl, basesatmeh2, cec7, cec82, ecec, phosphatephos, nitratenitrogen, ecmeasured, ecdeterminemeth, ec15, caco3equivmeasured, gypsumequiv, sodium, sar, gypsumreq, humiccolor, fulviccolor, humicfulviccolor, alummeasured, pyrophoshue, pyrophosvalue, pyrophoschroma, melanicindex\n  FROM phlabresults_View_1 \n  ORDER BY phiidref, sampledepthtop\n  ;"
  channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
  if (SS == FALSE) {
    q <- gsub(pattern = "_View_1", replacement = "", x = q, 
              fixed = TRUE)
  }
  d.phlabresults <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  d.phlabresults <- uncode(d.phlabresults)
  d.phlabresults <- within(d.phlabresults, {
    hzthk = sampledepthbottom - sampledepthtop
  })
  orig_names <- names(d.phlabresults)
  idx <- which(duplicated(d.phlabresults$phiidref))
  if (length(idx) < 0) {
    message(paste("NOTICE: multiple phiid values exist in the `phlabresults` table, computing a weighted averages and dominant values based on horizon thickness"))
    dup <- d.phlabresults[idx, "phiidref"]
    dup_idx <- which(d.phlabresults$phiidref %in% dup)
    d.dups <- d.phlabresults[dup_idx, ]
    num_vars <- names(d.dups)[!grepl("ph1to1h2o|ph01mcacl2", 
                                     names(d.dups)) & sapply(d.dups, is.numeric)]
    d.dups_num <- d.dups[num_vars]
    var <- "phiidref"
    d.dups_num <- do.call("rbind", by(d.dups_num, d.dups_num[var], 
                                      function(x) {
                                        data.frame(x[var][1, , drop = FALSE], lapply(x[2:ncol(x)], 
                                                                                     function(x2) weighted.mean(x2, w = x$hzthk, 
                                                                                                                na.rm = TRUE)))
                                      }))
    char_vars <- names(d.dups)[names(d.dups) %in% c("phiidref", 
                                                    "hzthk") | sapply(d.dups, function(x) is.character(x) | 
                                                                        is.factor(x))]
    d.dups_char <- d.dups[char_vars]
    d.dups_char <- do.call("rbind", by(d.dups_char, d.dups_char[var], 
                                       function(x) {
                                         data.frame(x[var][1, , drop = FALSE], lapply(x[2:ncol(x)], 
                                                                                      function(x2) x2[which.max(x$hzthk)]))
                                       }))
    d.dups_char$hzthk <- NULL
    num_ph <- names(d.dups)[names(d.dups) %in% c("phiidref", 
                                                 "hzthk") | grepl("ph1to1h2o|ph01mcacl2", names(d.dups))]
    d.dups_ph <- d.dups[num_ph]
    d.dups_ph <- do.call("rbind", by(d.dups_ph, d.dups_ph[var], 
                                     function(x) {
                                       data.frame(x[var][1, , drop = FALSE], lapply(x[2:ncol(x)], 
                                                                                    function(x2) -log10(weighted.mean(1/10^x2, 
                                                                                                                      weights = x$hzthk, na.rm = TRUE))))
                                     }))
    d.dups_ph$hzthk <- NULL
    d.nodups <- merge(d.dups_num, d.dups_char, by = "phiidref", 
                      all.x = TRUE)
    d.nodups <- merge(d.nodups, d.dups_ph, by = "phiidref", 
                      all.x = TRUE)
    d.nodups <- d.nodups[orig_names]
    d.phlabresults <- rbind(d.phlabresults[-dup_idx, ], 
                            d.nodups)
  }
  d.phlabresults[c("sampledepthtop", "sampledepthbottom", 
                   "hzthk")] <- NULL
  names(d.phlabresults) <- gsub("measured|lab$", "", names(d.phlabresults))
  vars <- c("phiidref")
  idx <- names(d.phlabresults) %in% vars
  names(d.phlabresults)[idx] <- c("phiid")
  names(d.phlabresults)[!idx] <- paste0(names(d.phlabresults)[!idx], 
                                        "_lab")
  d.phlabresults <- d.phlabresults[rowSums(is.na(d.phlabresults)) < 
                                     (length(d.phlabresults) - 1), ]
  RODBC::odbcClose(channel)
  return(d.phlabresults)
}
