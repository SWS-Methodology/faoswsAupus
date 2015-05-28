##' Standardize Node
##' 
##' Standardization follows the below process:
##' \itemize{
##'     \item Compute a matrix ("reverseMatrix" in the code) describing how
##'     much of a particular quantity is passed from a child to it's parent.
##'     The (i,j)-th entry of this matrix specifies a conversion factor for
##'     transferring element j quantities into element i quantities.
##'     \item Compute the matrix of required elements (column) for each
##'     commodity (row), and call this "valueMatrix".
##'     \item Compute the new value for each commodity by pre-multiplying
##'     valueMatrix by reverseMatrix and therefore aggregating children
##'     commodities into their parents.
##'     \item Lastly, save this new data back to the passed graph object, and
##'     return.
##' }
##'
##' @param graph The graph object created by the function constructGraph.
##' @param workingNode The nodes to be standardized
##' @param standardizeElement The attribute of the nodes to be
##' standardized.
##' @param productionElement The column name of the production element in the
##'   nodes of the graph.  Important if standardizeProduction = FALSE.
##' 
##' @return A graph object that has been updated according to the description
##' above.
##' 
##' @export
##' 

standardizeNode = function (graph, workingNode, standardizeElement,
                            productionElement = paste0(aupusParam$keyNames$valuePrefix,
                                                       aupusParam$keyNames$elementName,
                                                       "_51")){

    ## Get the edges and the construct the reverse matrix
    outEdges = E(graph)[from(V(graph)[workingNode])]
    ## shareMatrix/rateMatrix contains the values on the edges from parent to
    ## child, but in a matrix instead of a graph.  They are also
    ## subsetted based on the current working nodes to save memory.
    ## Divide shareMatrix by 100, as it's expressed as a percent
#     shareMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
#         sparse = FALSE, attr = "Value_share") / 100
    ## Divide rateMatrix by 10000, as it's expressed as a percent and scaled
    ## within the database.
    rateMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_extraction") / 10000
    ## Define the shares to be 1 if a single connection exists.  However, if a
    ## node has multiple parents, give each parent an equal proportion of the
    ## share.
    shareMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE)
    shareMatrix = shareMatrix * 1/apply(shareMatrix, 1, sum)
    reverseMatrix = t(shareMatrix)/t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    ## reverseMatrix allows us to transfer quantities from a child to it's
    ## parent.  However, the parent values should all remain fixed, so place
    ## a value of 1 in the (i,i) element of reverseMatrix whenever i is a 
    ## parent node.
    parentNode = rownames(reverseMatrix)[!rownames(reverseMatrix) %in%
                                             workingNode]
    diag(reverseMatrix)[parentNode] = 1
    ## NA values or infinities indicate no transfer, so set to 0.
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    
    ## Extract the values which are to be standardized from the vertices
    ## of the graph.
    valueMatrix = matrix(unlist(lapply(
            X = standardizeElement,
            FUN = function(x){
                get.vertex.attribute(graph = graph, name = x,
                                     index = V(graph)[colnames(shareMatrix)])
            })),
        ncol = length(standardizeElement))
    colnames(valueMatrix) = standardizeElement

    ## Standardize children up to parents
    standardizedMatrix = reverseMatrix %*% ifelse(is.na(valueMatrix), 0, valueMatrix)
    ## Production is not intended to be standardized within the commodity trees,
    ## so in those cases replace that column of standardizedMatrix with the
    ## original column of valueMatrix.  However, when working with aggregate
    ## elements (i.e. FBS elements), standardization of production is required.
    rowFilter = !grepl("S", rownames(standardizedMatrix))
    standardizedMatrix[rowFilter, colnames(standardizedMatrix) == productionElement] =
        valueMatrix[rowFilter, colnames(valueMatrix) == productionElement]

    ## First, save the current state of all the nodes that are "working nodes",
    ## i.e. all the nodes which are being standardized to the next level.  Their
    ## information will soon be aggregated up in the graph, and so we want to
    ## save their current state somewhere before that happens.
    intermediateValuesMatrix =
        do.call("cbind",
                lapply(X = standardizeElement,
                       FUN = function(x){
                           get.vertex.attribute(graph = graph, name = x,
                                                index = V(graph)[workingNode])
                           }))
    intermediateValuesMatrix = data.frame(intermediateValuesMatrix)
    colnames(intermediateValuesMatrix) = standardizeElement
    intermediateValuesMatrix$measuredItem = workingNode
    
    ## Now, we can update the graph with the new, standardized values.
    for(i in 1:length(standardizeElement)){
        graph = set.vertex.attribute(graph = graph,
                 name = standardizeElement[i],
                 index = V(graph)[rownames(standardizedMatrix)],
                 value = standardizedMatrix[, i])
    }

    ## Delete the standardized workingNodes
    graph = graph - vertices(workingNode)

    ## Return the objects
    list(standardizedGraph = graph, intermediateValues = intermediateValuesMatrix)
}
