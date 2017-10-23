

\name{genVarUI}
\alias{genVarUI}
\title{Generate Variable List}
\arguments{
  \item{Identifier Var}{A character vector with the identifier variable of the inclusion criterion. Notice that it could be multiple character strings,it also could be empty. And the default value is empty.}
  
  \item{Table}{A character string with the name of database. If the type of data table is the ORE, Oracle and SQL, there is no need to input the extension, for example, "table_name"; If the type of data table is the flat type, there should be add the extension in, for example,"table_name_1.csv"}
  
\item{Omit var}{A character vector with the omit variables of the variable list. It could be multiple character strings. It also could be "", when the omit.var equal to "" means no variable need to be omitted. And the default value is "".}
  
  \item{Overwrite}{A logical value whether rewrite the inclusion criteria when it already exists. The default value is TRUE.}
  
}
\description{
Users should have access to the database which is used to generate the variable list. And DaSA team generates the variable list by this function.
}
\value{
Variable list with two sheets,the first is overall list, its column contains: variable,
variable's description (if applicable), remarks and selection; the second is setting list, its colum contains:
Argument and Specification.
}



