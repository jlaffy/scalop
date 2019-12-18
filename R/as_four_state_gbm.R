
#' @title Convert 6-state to 4-state GBM scores
#' @description 6 states: MES1, MES2, NPC1, NPC2, AC, OPC. 4 states: MES, NPC, AC, OPC. X1/2 are converted to X simply by taking the maximum score of the two columns.
#' @param scores dataframe of cell (rows) scores for the GBM meta-modules (columns).
#' @return the same dataframe with the MES1 and MES2, and NPC1 and NPC2 columns collapsed to MES and NPC respectively.
#' @rdname as_four_state_gbm
#' @export 
#' @importFrom dplyr transmute
as_four_state_gbm = function(scores) {
    scores = as.data.frame(scores)
    s = scores %>% dplyr::transmute(MES = pmax(MES1, MES2),
                                    AC = AC,
                                    OPC = OPC,
                                    NPC = pmax(NPC1, NPC2))
    rownames(s) = rownames(scores)
    s
}

