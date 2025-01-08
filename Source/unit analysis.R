# Unit -----
Unit <- dbGetQuery(conDuck, "SELECT * FROM Unit")

unit_analysis <- Unit |> 
  filter(ColumnName != "")

unit.table <- gt(unit_analysis) |>
  tab_header(
    title = md("**Measurement units**")) |>
   cols_hide(columns = c(UnitID,ColumnName)) |>
   cols_label(
    UnitNameDE = 'German',
    UnitNameEN = 'English',
    UnitSymbol = "Symbol")
