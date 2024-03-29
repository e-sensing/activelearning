% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s2.R
\name{.al_s2_short_paths}
\alias{.al_s2_short_paths}
\title{Get the shortest paths between vertices.}
\usage{
.al_s2_short_paths(from_vertex, to_vertices, G)
}
\arguments{
\item{to_vertices}{An integer. The indices of vertices in G.}

\item{G}{An igraph object.}

\item{fom_vertex}{A single integer. The index of a vertex in G.}
}
\value{
\preformatted{       A list of two: The shortest paths' lengths, and
               the vertex id of their mid points.
}
}
\description{
This function computes the paths from a vertex to a set of
vertices in graph.
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}
\keyword{internal}
