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
    if(is(cwl, "cwlWorkflow")){
        mtime <- mcols(cwlSearch(paste0("pl_", nn)))$mtime
    }else{
        mtime <- mcols(cwlSearch(paste0("tl_", nn)))$mtime
    }
    
    dir.create(plotdir, showWarnings = FALSE)
    pl <- plotCWL(cwl)
    write(export_svg(pl), file.path(plotdir, paste0(nn, ".svg")))

    inputs <- paste(kable(do.call(rbind, mt$inputs)), collapse = "\n")
    outputs <- paste(kable(do.call(rbind, mt$outputs)), collapse = "\n")

    header <- paste("---", paste("title:", mt$label),
                    paste("description:", des),
                    paste("Author:", cwl@extensions$author),
                    paste("Last updated:", sub("\\s.*", "", mtime)),
                    "---", sep = "\n")
    
    md <- paste(header, title, des,
                "## plot",
                paste0("![", title, "](/", plotdir, "/",nn, ".svg", ")"),
                "## Inputs", inputs,
                "## Outputs", outputs, sep = "\n")

    if(!is.null(mt$steps)){
        steps <- paste(kable(do.call(rbind, mt$steps)), collapse = "\n")
        md <- paste(md, "## steps", steps, sep = "\n")
    }
    md <- gsub("character\\(0\\)", " ", md)
    return(md)
}
