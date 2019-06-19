

## Prepping the Data

0. Download the reports to a local directory with a wrapper function for `download.file()` called `download_report()`.
1. Use `purrr::map()` and `pdftools::pdf_text()` to import the raw text for each report (with `year` as a grouping variable) to create `pages_nest`. This tibble (or data frame) has each report stored as a list column named `page`.
2. Add an index `idx_page` (via `dplyr::row_number()`) and "unnest" the `page` column of `pages_nest` to create `pages`.
  + The index `idx_page` doesn't correspond directly to the page numbers of the reports because the reports have various type of pages leading up to the first numbered page---title page, blank pages, a table of contents, and an exective summary. The numbered pages in each report begin somewhere in the 30s---it varies for each report---in terms of the "raw" PDF page number.
  + This tibble has each page's raw text stored an individual row. Of course, this is quite messy. For example, the second page of the executive summary reads like `"    Executive Summary\r\n    Review of Real-Time Market Outcomes\r\n    Although only a small share [...]"`.
3. Split `pages` into the the variables `toc_pages` and `body_pages`, which--in case it isn't obvious--represent the table of contents and body or the report.
4. Create a data frame `body_rngs` to "map" the page numbers to each report.
  + A data frame `pages_n` is created beforehand to aid with the mapping.


## Analyzing the Table of Contents of Each Report

1. "Munge"`toc_pages` using a custom function `convert_page_to_lines()` to create the data frame `toc` with rows corresponding to each line of the table of contents sections.
  + These rows look like ~~`"executive summary - i"`, `"a- summary of congestion - 45"`, `"figure 1: average all-in price for electricity in ercot - 2"`, `"table 1: average annual real-time energy market prices by zone - 4"`.~~

```{r toc_ex_show-1, eval=F}
toc_ex
```

  + Note that the `convert_page_to_lines()` function is key to this cleaning step. It uses an idiom that seems like it could be applied to many kinds of text parsing---namely, `stringr::str_split(x, "\\n") %>% purrr::map(stringr::str_squish) %>% unlist() %>% tibble::enframe()`.

2. "Augment" and "clean" the `toc` data frame with columns for `label`, `index` (the , and `page_num`.
  + For example

```{r toc_aug_ex_show-1, eval=F}
toc_aug_ex
```

  + Initially, I had hoped to use an approach like that demonstrated by #rstats aficionado [`hrbrmstr`]() in [this post about parsing pdf tables](https://rud.is/b/2017/01/26/one-view-of-the-impact-of-the-new-immigration-ban-freeing-pdf-data-with-tabulizer/), but I concluded that the data was too "irregular" to warrant this approach. Thus, I turned to every data scientist's/software engineer's best friend---[regular expressions](https://en.wikipedia.org/wiki/Regular_expression)! (Please acknowledge the sarcasm :).) Thankfully, the data was uniform enough to be parsed in this manner.

## Analyzing the Body of the Reports
