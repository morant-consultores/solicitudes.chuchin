## code to prepare `data_ejemplo` dataset goes here

ex_data <- solicitudes |>
  mutate(estatus = sample(c("recibida", "en proceso", "finalizada", "rechazada"), nrow(solicitudes),
                          replace = T, prob = rep(0.25, 4)),
         random_offset = sample(10000:25000, nrow(solicitudes), replace = TRUE),
         fecha_proceso = if_else(estatus == "en proceso", fecha + random_offset, fecha_proceso),
         fecha_rechazo = if_else(estatus == "rechazada", fecha + random_offset, fecha_rechazo),
         fecha_atendida = if_else(estatus == "finalizada", fecha + random_offset, fecha_atendida)) |>
  select(-random_offset) |>
  readr::write_csv("inst/data_ejemplo/ex_data.csv")
