#' Meta information to markdown
#'
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom knitr kable
#' @export

meta2md <- function(cwl, plotdir = "plots"){
    mt <- meta(cwl)
    title <- paste("##", mt$label)
    des <- mt$doc

    mtime <- mt$extensions$`$rud`$date  ## for data recipe

    nn <- deparse(substitute(cwl))
    if (is.null(mtime)) {
        if(is(cwl, "cwlWorkflow")){
            mtime <- mcols(cwlSearch(paste0("pl_", nn)))$mtime
        }else{
            mtime <- mcols(cwlSearch(paste0("tl_", nn)))$mtime
        }
    }
    ## plot
    dir.create(plotdir, showWarnings = FALSE)
    pl <- plotCWL(cwl)
    write(export_svg(pl), file.path(plotdir, paste0(nn, ".svg")))

    inputs <- paste(knitr::kable(do.call(rbind, mt$inputs)), collapse = "\n")
    outputs <- paste(knitr::kable(do.call(rbind, mt$outputs)), collapse = "\n")

    header <- paste("---", paste("title:", mt$label),
                    paste("description:", des),
                    paste("Author:", cwl@extensions$`$rud`$author),
                    paste("Last updated:", sub("\\s.*", "", mtime)),
                    "---", sep = "\n")

    if(!is.null(mt$extensions$`$rud`$url)) {  ## add data source url for data recipe (ReUseData)
        des <- paste0(des, "\n", "Data source: ", paste0("<", mt$extension$`$rud`$url, ">", collapse = "; "))
    }

    md <- paste(header, title, des,
                "## plot",
                paste0("![", title, "](/", plotdir, "/",nn, ".svg", ")"),
                "## Inputs", inputs,
                "## Outputs", outputs, sep = "\n")

    if(!is.null(mt$steps)){
        steps <- paste(knitr::kable(do.call(rbind, mt$steps)), collapse = "\n")
        md <- paste(md, "## steps", steps, sep = "\n")
    }
    md <- gsub("character\\(0\\)", " ", md)

    if(!is.null(mt$extension$`$rud`$example)) {  ## add example code for data recipe (ReUseData)
        md <- paste(md, "## Example:", "```", mt$extension$`$rud`$example, "```\n", sep="\n")
    }
    
    return(md)
}
