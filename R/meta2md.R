#' meta2md
#' Meta information to markdown
#' @param cwl the `cwlProcess` or `cwlWorkflow` object
#' @param outdir the output file path for the to-be-generated figure and md files
#' @param prefix the filename for to-be-generated .svg file and .md file
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom knitr kable
#' @export

meta2md <- function(cwl, outdir = ".", prefix = deparse(substitute(cwl))){
    mt <- meta(cwl)
    title <- paste("##", mt$label)
    des <- mt$doc

    mtime <- mt$extensions$`$rud`$date  ## for data recipe

    if (is.null(mtime)) {
        if(is(cwl, "cwlWorkflow")){
            mtime <- mcols(cwlSearch(paste0("pl_", prefix)))$mtime
        }else{
            mtime <- mcols(cwlSearch(paste0("tl_", prefix)))$mtime
        }
    }
    ## plot
    dir.create(outdir, showWarnings = FALSE)
    pl <- plotCWL(cwl)
    write(export_svg(pl), file.path(outdir, paste0(prefix, ".svg")))

    ## table for inputs/outputs 
    itypes <- lapply(inputs(cwl), function(x) paste(x@type, collapse=";"))
    itable <- cbind(do.call(rbind, mt$inputs), do.call(rbind, itypes))
    itable <- itable[, c(1,3,2), drop=FALSE]
    otypes <- lapply(outputs(cwl), function(x) paste(x@type, collapse=";"))
    otable <- cbind(do.call(rbind, mt$outputs), do.call(rbind, otypes))
    otable <- otable[, c(1,3,2), drop=FALSE]
    colnames(otable) <- c("label", "type", "description")
    outputs <- paste(knitr::kable(otable), collapse = "\n")

    if (!is.null(itable)) {
        colnames(itable) <- colnames(otable)
        inputs <- paste(knitr::kable(itable), collapse = "\n")
    }

    ## header
    header <- paste("---", paste("title:", gsub("_", " ", mt$label)),
                    paste("description:", des),
                    paste("Author:", cwl@extensions$`$rud`$author),
                    paste("Last updated:", sub("\\s.*", "", mtime)),
                    paste("type: article"),
                    "---", sep = "\n")
    
    ## add data source url for data recipe (ReUseData)
    if(!is.null(mt$extensions$`$rud`$url)) { 
        des <- paste0(des, "<br>Data source: ", paste0("<", mt$extension$`$rud`$url, ">", collapse = "; "))
    }

    md <- paste(header, title, des,
                "## plot",
                paste0("![", title, "](/plots/",prefix, ".svg", ")"),
                "## Inputs", ifelse(!is.null(itable), inputs, ""),
                "## Outputs", outputs, sep = "\n")

    ## add step info for workflow recipes (RcwlPipelines)
    if(!is.null(mt$steps)){
        steps <- paste(knitr::kable(do.call(rbind, mt$steps)), collapse = "\n")
        md <- paste(md, "## steps", steps, sep = "\n")
    }
    md <- gsub("character\\(0\\)", " ", md)

    ## add example code for data recipes (ReUseData)
    if(!is.null(mt$extension$`$rud`$example)) { 
        md <- paste(md, "## Example:", "```", mt$extension$`$rud`$example, "```\n", sep="\n")
    }
    write(md, file = file.path(outdir, paste0(prefix, ".md")))
    return(md)
}
