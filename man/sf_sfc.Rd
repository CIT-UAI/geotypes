% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf.R
\name{sf_sfc}
\alias{sf_sfc}
\title{SF: Geometry Column Type}
\usage{
sf_sfc(
  types = NULL,
  only_valid = NULL,
  point_dims = NULL,
  uniform_dim = NULL,
  ...
)
}
\arguments{
\item{types}{Vector with possible acceptable geometry types}

\item{only_valid}{TRUE to accept sfc only with valid geometries}

\item{point_dims}{A vector which declares how much dimensions are accepted}

\item{uniform_dim}{The sfc can only have geometries of the same dimension}

\item{...}{Parsed to assertion_factory}
}
\value{
Assertion for sfg
}
\description{
typed for geometry column types from SF
}
