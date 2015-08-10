##' Process Forward
##' 
##' A few edges in the commodity trees are labeled with an 'F' to indicate that 
##' processing is 'forward'.  The parent commodities in these edges are 
##' immediately converted into the corresponding child and then they are removed
##' from the tree (as we will standardize to the children instead).  This is a 
##' rare scenario; an example commodity is sugar.
##' 
##' Note: when commodities are processed forward like this, the final flag is
##' assigned ARBITRARILY as the first flag observed (for lack of a better
##' approach).  This should be corrected.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return A list of names 'data' and 'tree'.  Both objects must be returned, 
##'   as the tree is updated by pruning off some edges.
##'   

processForward = function(data, tree, standParams){
    
    ## If no forward processing edge, than don't do anything:
    if(all(tree[, get(standParams$targetVar) != "F"])){
        return(list(data = data, tree = tree))
    }
    
    cnames = colnames(data)
    
    subTree = tree[get(standParams$targetVar) == "F", ]
    level = getCommodityLevel(subTree, parentColname = standParams$parentVar,
                              childColname = standParams$childVar)
    setnames(level, c(standParams$parentVar, "level"))
    if(length(unique(tree[parentID %in% subTree[, parentID], target])) > 1){
        warning("Some parents have one edge set to be forward processed and ",
                "another edge not.  How to handle such a case is not clear, ",
                "and this may cause strange behavior.")
    }
    subTree = merge(subTree, level, by = standParams$parentVar)
    setnames(subTree, standParams$parentVar, standParams$itemVar)
    
    ## Use the mode as the flag aggregation algorithm
    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    
    for(currentLevel in subTree[, sort(unique(level))]){
        data = merge(data, subTree[level == currentLevel, ], all.x = TRUE,
                     by = standParams$itemVar, allow.cartesian = TRUE)
        ## Process the node down:
        data[!is.na(get(standParams$extractVar)), c(standParams$itemVar) :=
                 get(standParams$childVar)]
        data[!is.na(get(standParams$extractVar)), Value :=
                 Value * get(standParams$extractVar)]
        data[!is.na(get(standParams$extractVar)), standardDeviation :=
                 standardDeviation * get(standParams$extractVar)]
        
        ## Don't overwrite official production, if it's available:
        data[element == standParams$productionCode,
             officialProduction := sum(!is.na(Value) &
                is.na(get(standParams$extractVar))) > 0,
             by = c(standParams$mergeKey)]
        ## To filter out values that would overwrite official production, we
        ## remove values that correspond to production elements, have
        ## non-missing extraction rates and have official production.
        data = data[!(element == standParams$productionCode &
                          !is.na(get(standParams$extractVar)) &
                          officialProduction), ]
        
        ## Should aggregate the flags too:
        data = data[, list(Value = sum(Value),
                           standardDeviation =
                               sqrt(sum(standardDeviation^2)),
                           metFlag = Mode(metFlag),
                           obsFlag = Mode(obsFlag)),
             by = c(standParams$mergeKey, "element")]
    }
    tree = tree[!get(standParams$targetVar) == "F", ]
    return(list(data = data, tree = tree))
}