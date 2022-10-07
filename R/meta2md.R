#' Meta information to markdown
#'
#' @importFrom DiagrammeRsvg export_svg
#' @export

meta2md <- function(cwl, plotdir = "plots"){
    mt <- meta(cwl)
    title <- paste("##", mt$label)
    des <- mt$doc

    ## plot
    nn <- deparse(substitute(cwl))
    dir.create(plotdir, showWarnings = FALSE)
    pl <- plotCWL(cwl)
    write(export_svg(pl), file.path(plotdir, paste0(nn, ".svg")))

    inputs <- paste(kable(do.call(rbind, mt$inputs)), collapse = "\n")
    outputs <- paste(kable(do.call(rbind, mt$outputs)), collapse = "\n")
    md <- paste(title, des,
                "## plot",
                paste0("![Alt text](", file.path(plotdir, paste0(nn, ".svg")), ")"),
                "## Inputs", inputs,
                "## Outputs", outputs, sep = "\n")

    if(!is.null(mt$steps)){
        steps <- paste(kable(do.call(rbind, mt$steps)), collapse = "\n")
        md <- paste(md, "## steps", steps, sep = "\n")
    }
    return(md)
}
