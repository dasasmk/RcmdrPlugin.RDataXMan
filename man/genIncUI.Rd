

\name{genIncUI}
\alias{genIncUI}
\title{Generate Inclusion Criteria}

\arguments{
  \item{Key Variable}{A character string with the key variable of the inclusion criterion. It should be one character.}
  
  \item{Key Description}{A character vector with the key description of the inclusion criterion. It could be multiple character strings,
it also could be empty. And the default value is empty.}
  
  \item{Table}{A character string with the name of database. If the type of data table is the ORE, Oracle and SQL,
there is no need to input the extension, for example, "table_name"; If the type of data table is the flat type,
there should be add the extension in, for example,"table_name_1.csv"}
  
  \item{Identifier Variable}{A character vector with the identifier variableof the inclusion criterion. Notice that it could be multiple character strings,
it also could be empty. And the default value is empty.}

  \item{Count Identifier Variable}{A logical vector indicating whether a count summary will be generated for each of the variables specified in identifier.var.
It could be all FALSE, means generate inclusion criteria without count information. The default value is rep(FALSE,length(identifier.var)).}

  \item{Overwrite}{A logical value whether rewrite the inclusion criteria when it already exists. The default value is TRUE.}

}
\description{
Users should have access to the database which is used to generate the inclusion criterion. And DaSA team generates the inclusion criterion by this function.
}
\value{
Inclusion criterion with two sheets, the first is overall list, its column contains: key variable,
key variable's description (if applicable), remarks and selection; the second is setting list, its colum contains:
Argument and Specification.
}

