
#' @export
meta <- function(cwl){
    idoc <- lapply(cwl@inputs, function(x)list(label = x@label, doc = x@doc))
    odoc <- lapply(cwl@outputs, function(x)list(label = x@label, doc = x@doc))
    if(is(cwl, "cwlWorkflow")){
        sdoc <- lapply(cwl@steps, function(x)list(label = x@label, doc = x@doc))
        metalist <- list(label = cwl@label,
                         doc = cwl@doc,
                         inputs = idoc,
                         outputs = odoc,
                         steps = sdoc,
                         extensions = cwl@extensions)
    }else{
        metalist <- list(label = cwl@label,
                         doc = cwl@doc,
                         inputs = idoc,
                         outputs = odoc,
                         extensions = cwl@extensions)
    }
    return(metalist)
}

#' @export
"meta<-" <- function(cwl, value){
    if(!is.null(value$label))
        cwl@label <- value$label    
    if(!is.null(value$doc))
        cwl@doc <- value$doc
    if(!is.null(value$extensions))
        cwl@extensions <- value$extensions

    if(length(value$inputs)>0){
        idx <- match(names(value$inputs), names(inputs(cwl)))
        for(i in idx){
            cwl@inputs[[i]]@doc <- value$inputs[[i]]$doc
            cwl@inputs[[i]]@label <- value$inputs[[i]]$label
        }
    }
    if(length(value$outputs) > 0){
        idx <- match(names(value$outputs), names(outputs(cwl)))
        for(i in idx){
            cwl@outputs[[i]]@doc <- value$outputs[[i]]$doc
            cwl@outputs[[i]]@label <- value$outputs[[i]]$label
        }
    }
    if(length(value$steps) > 0){
        idx <- match(names(value$steps), names(steps(cwl)))
        for(i in idx){
            cwl@steps[[i]]@doc <- value$steps[[i]]$doc
            cwl@steps[[i]]@label <- value$steps[[i]]$label
        }
    }
    return(cwl)
}

#' @export
addMeta <- function(cwl, label, doc,
                    inputLabels = character(),
                    inputDocs = character(),
                    outputLabels = character(),
                    outputDocs = character(),
                    stepLabels = character(),
                    stepDocs = character(),
                    extensions = list()){
    stopifnot(length(inputLabels) == length(inputDocs))
    stopifnot(length(outputLabels) == length(outputDocs))
    stopifnot(length(stepLabels) == length(stepDocs))
    ## inputs
    ilist <- list()
    for(i in seq(inputLabels)){
        ilist[[i]] <- list(label = inputLabels[i], doc = inputDocs[i])
    }
    
    if (all(!is.null(names(inputLabels)))){
        names(ilist) <- names(inputLabels)
    }else if (length(inputLabels) == length(inputs(cwl))){
        names(ilist) <- names(inputs(cwl))
    }
    ## outputs
    olist <- list()
    for(i in seq(outputLabels)){
        olist[[i]] <- list(label = outputLabels[i], doc = outputDocs[i])
    }

    if (all(!is.null(names(outputLabels)))){
        names(olist) <- names(outputLabels)
    }else if (length(outputLabels) == length(outputs(cwl))){
        names(olist) <- names(outputs(cwl))
    }
    ## steps
    slist <- list()
    if(is(cwl, "cwlWorkflow")){
        for(i in seq(stepLabels)){
            slist[[i]] <- list(label = stepLabels[i], doc = stepDocs[i])
        }
        
        if (all(!is.null(names(stepLabels)))){
            names(slist) <- names(stepLabels)
        }else if (length(stepLabels) == length(steps(cwl))){
            names(slist) <- names(steps(cwl))
        }
    }
    meta(cwl) <- list(label = label,
                      doc = doc,
                      inputs = ilist,
                      outputs = olist,
                      steps = slist,
                      extensions = extensions)
    return(cwl)
}
