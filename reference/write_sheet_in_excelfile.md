# Append a data frame as a new sheet in an Excel workbook

Writes a data frame to a new worksheet, creating the workbook if it does
not yet exist and otherwise appending to it.

## Usage

``` r
write_sheet_in_excelfile(df, filename, sheetName)
```

## Arguments

- df:

  A data frame to write.

- filename:

  Path of the `.xlsx` file.

- sheetName:

  Name of the worksheet to add.

## Value

Invisibly returns `filename`.
