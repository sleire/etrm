## Second submission

> please omit the redundant "in R" from the title.

The "in R" part has been removed.

> The Description field is intended to be a (one paragraph) description of
what the package does and why it may be useful.
Please add more details about the package functionality and implemented
methods in your Description text.

The Description field has been updated with a more detailed presentation of the package contents.

> Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles.
e.g.:  F. E. Benth, S. Koekkebakker, and F. Ollmar
Please explain in the submission comments what you did about this issue.

The etrm package implements methods from financial theory, and the most relevant
papers are listed as references in the DESCRIPTION file. The referenced authors
have developed the theoretical models (such as the Black-76 option pricing model or 
the maximum smoothness forward curve approach), but they have not directly participated 
in the development of the etrm package. For this reason we found it natural to acknowledge 
these contributions by citing the scientific papers, and no via co-authorship. We would be
grateful to receive guidance, if this should be solved in a different manner.

> If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

The references have been included in the DESCRIPTION file.

> Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      plot-GenericStrat-method.Rd: \value
      plot-MSFC-method.Rd: \value
      show-GenericStrat-method.Rd: \value
      show-MSFC-method.Rd: \value
      summary-GenericStrat-method.Rd: \value
      summary-MSFC-method.Rd: \value
      
The description of exported methods have been updated in accordance with requirements above.


## Initial CRAN submission

## Test environments
* local R installation, R 4.0.5
* ubuntu 16.04 (on travis-ci), R 4.0.5
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
