#' SENNA socio-emotional skills item responses
#'
#' Item responses to the SENNA inventory of social and emotional skills,
#' collected from Brazilian students. The inventory uses balanced scales (paired
#' positively and negatively keyed items) so that acquiescence can be estimated
#' and corrected. Most items are answered on a 1-5 Likert scale.
#'
#' @format A data frame with 2,300 rows and 205 columns:
#'
#'   * `ExamineeID`, `TestingSessionID` --- respondent and testing-session
#'     identifiers.
#'   * 195 item-response columns (such as `C001_1`, `sv001.v2` or `sv2.725`)
#'     whose names match the `coditem` values in [senna_dic]. Most items use a
#'     1--5 Likert scale.
#'   * `age1`, `Education`, `sex`, `ses`, `educ_oprtnty`, `race_min` ---
#'     demographic variables.
#'   * `profic_lp_15`, `profic_mat_15` --- Portuguese-language and mathematics
#'     proficiency criteria.
#' @source SENNA project, Ayrton Senna Institute / EduLab21.
#' @seealso [senna_dic]
"data_senna"

#' SENNA item dictionary
#'
#' Metadata describing the SENNA items, including the scale each item is scored
#' on, its keying direction, and the semantic pair it belongs to. This is the
#' `item_dic` argument expected by [recode_for_acq()].
#'
#' @format A data frame with 165 rows and 10 columns:
#' \describe{
#'   \item{coditem}{Item code; matches a column name in [data_senna].}
#'   \item{scale}{Scale the item is scored on (the Big Five domains A, C, E, N,
#'     O, plus an overclaiming scale OvCl).}
#'   \item{pole}{Keying direction: `1` positively keyed (high pole), `0`
#'     negatively keyed (low pole).}
#'   \item{seman_pairs}{Identifier of the semantic pair the item belongs to;
#'     `NA` for unpaired items.}
#'   \item{item_text}{Item text in English.}
#'   \item{item_text_pt}{Item text in Portuguese.}
#'   \item{domain}{Higher-order domain the scale belongs to.}
#'   \item{facet}{Facet within the domain.}
#'   \item{pers_selfeff}{Indicator distinguishing personality from
#'     self-efficacy framed items.}
#'   \item{seman_pairs0}{An earlier version of the semantic-pair coding.}
#' }
#' @source SENNA project, Ayrton Senna Institute / EduLab21.
#' @seealso [data_senna]
"senna_dic"
