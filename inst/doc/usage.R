## ----echo=FALSE, include=FALSE------------------------------------------------
library(rmarkdown)
library(sdcHierarchies)
library(data.table)

## ----eval = FALSE-------------------------------------------------------------
# library(sdcHierarchies)

## -----------------------------------------------------------------------------
h <- hier_create(root = "Total", nodes = LETTERS[1:5])
hier_display(h)

## -----------------------------------------------------------------------------
## adding nodes below the node specified in argument `node`
h <- hier_add(h, root = "A", nodes = c("a1", "a2"))
h <- hier_add(h, root = "B", nodes = c("b1", "b2"))
h <- hier_add(h, root = "b1", nodes = c("b1_a", "b1_b"))

# deleting one or more nodes from the hierarchy
h <- hier_delete(h, nodes = c("a1", "b2"))
h <- hier_delete(h, nodes = c("a2"))

# rename nodes
h <- hier_rename(h, nodes = c("C" = "X", "D" = "Y"))
hier_display(h)

## -----------------------------------------------------------------------------
# about a specific node
info <- hier_info(h, nodes = c("b1", "E"))

## -----------------------------------------------------------------------------
info$b1

## -----------------------------------------------------------------------------
# conversion to a "@;label"-based format
res_df <- hier_convert(h, as = "df")
print(res_df)

## -----------------------------------------------------------------------------
code <- hier_convert(h, as = "code"); cat(code, sep = "\n")

## ----eval=FALSE---------------------------------------------------------------
# hier_export(h, as = "argus", path = file.path(tempdir(), "hierarchy.hrc"))

## -----------------------------------------------------------------------------
n_df <- hier_import(inp = res_df, from = "df")
hier_display(n_df)

## -----------------------------------------------------------------------------
geo_m <- c(
  "01051", "01053", "01054", "01055", "01056", "01057", "01058", "01059", "01060", "01061", "01062",
  "02000",
  "03151", "03152", "03153", "03154", "03155", "03156", "03157", "03158", "03251", "03252", "03254", "03255",
  "03256", "03257", "03351", "03352", "03353", "03354", "03355", "03356", "03357", "03358", "03359", "03360",
  "03361", "03451", "03452", "03453", "03454", "03455", "03456",
  "10155")

## -----------------------------------------------------------------------------
# Using end positions (e.g., level 1 ends at index 2, level 2 at 3, level 3 at 5)
v1 <- hier_compute(
  inp = geo_m, 
  dim_spec = c(2, 3, 5), 
  root = "Tot", 
  method = "endpos", 
  as = "df"
)

# Using lengths (e.g., level 1 is 2 chars, level 2 is 1 char, level 3 is 2 chars)
v2 <- hier_compute(
  inp = geo_m, 
  dim_spec = c(2, 1, 2), 
  root = "Tot", 
  method = "len",
  as = "df"
)

identical(v1, v2)
hier_display(v1)

## -----------------------------------------------------------------------------
geo_m_with_tot <- paste0("Tot", geo_m)
head(geo_m_with_tot)

v3 <- hier_compute(
  inp = geo_m_with_tot, 
  dim_spec = c(3, 2, 1, 2), 
  method = "len"
)
hier_display(v3)

## -----------------------------------------------------------------------------
## Example with unequal string lengths; overall total provided via 'root'
yae_h <- c(
  "1.1.1.", "1.1.2.",
  "1.2.1.", "1.2.2.", "1.2.3.", "1.2.4.", "1.2.5.", "1.3.1.",
  "1.3.2.", "1.3.3.", "1.3.4.", "1.3.5.",
  "1.4.1.", "1.4.2.", "1.4.3.", "1.4.4.", "1.4.5.",
  "1.5.", "1.6.", "1.7.", "1.8.", "1.9.", "2.", "3.")

v1 <- hier_compute(
  inp = yae_h, 
  dim_spec = c(2, 2, 2), 
  root = "Tot", 
  method = "len"
)
hier_display(v1)

## -----------------------------------------------------------------------------
yae_ll <- list()
yae_ll[["Total"]] <- c("1.", "2.", "3.")
yae_ll[["1."]]    <- paste0("1.", 1:9, ".")
yae_ll[["1.1."]]  <- paste0("1.1.", 1:2, ".")
yae_ll[["1.2."]]  <- paste0("1.2.", 1:5, ".")
yae_ll[["1.3."]]  <- paste0("1.3.", 1:5, ".")
yae_ll[["1.4."]]  <- paste0("1.4.", 1:6, ".")

d <- hier_compute(inp = yae_ll, root = "Total", method = "list") 
hier_display(d)

## -----------------------------------------------------------------------------
h1 <- hier_create("Total", nodes = LETTERS[1:3])
h1 <- hier_add(h1, root = "A", node = "a1")
h1 <- hier_add(h1, root = "a1", node = "aa1")
hier_display(h1)

h2 <- hier_create("Total", letters[1:5])
h2 <- hier_add(h2, root = "b", node = "b1")
h2 <- hier_add(h2, root = "d", node = "d1")
hier_display(h2)

## -----------------------------------------------------------------------------
# cell_id is a unique string created by concatenating default codes
r <- hier_grid(h1, h2, add_dups = FALSE, add_levs = TRUE)
print(r)

## -----------------------------------------------------------------------------
# Create an SDC-optimized grid
r_sdc <- hier_grid(h1, h2, add_dups = FALSE, add_contributing_cells = TRUE)

# Genrate microdata using base-level codes for region and sector
# Note: 'aa1', 'b1', and 'd1' are the granular leaf nodes
microdata <- data.table(
  region = c("aa1", "B", "C", "aa1", "B"),
  sector = c("a", "b1", "c", "d1", "e"),
  val = c(10, 20, 30, 40, 50)
)

# Map microdata to base-level IDs using a named list
microdata[, leaf_id := hier_create_ids(
  data = microdata, 
  dims = list("region" = h1, "sector" = h2)
)]

print(microdata)

# Fast aggregation: Summing 'Total_Total' using integer lookups
total_ids <- r_sdc[v1 == "Total" & v2 == "Total", contributing_leaf_ids[[1]]]
print(total_ids)

sum(microdata[leaf_id %in% total_ids, val])

## -----------------------------------------------------------------------------
# Isolate primary cells for primary suppression
primary_cells <- r_sdc[!is.na(leaf_id)]

# Isolate aggregate cells for marginal consistency checks
sub_totals <- r_sdc[is.na(leaf_id)]

## ----eval=FALSE---------------------------------------------------------------
# # Start the app and store the modified result upon closing
# d_modified <- hier_app(d)

