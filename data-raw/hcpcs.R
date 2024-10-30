source(here::here("data-raw", "pins_internal.R"))

hcpcs_vec <- collapse::funique(
  c(
    northstar::search_descriptions()$hcpcs_code,
    northstar::search_aocs()$hcpcs_code,
    provider::betos(tidy = FALSE)$HCPCS_Cd
    ),
  sort = TRUE
  )

pin_update(
  hcpcs_vec,
  name = "hcpcs_vec",
  title = "HCPCS Codes",
  description = "Vector of HCPCS Codes for Testing"
)
