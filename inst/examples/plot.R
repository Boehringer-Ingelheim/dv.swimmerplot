library(dplyr)
library(ggplot2)

adsl <- pharmaverseadam::adsl |> head(20)

swimmer_data <- adsl |>
  filter(!is.na(TRTDURD)) |>
  arrange(TRTDURD) |>
  mutate(subject_order = row_number()) |>
  select(USUBJID, TRTDURD, subject_order, EOSSTT, ARM)

ggplot(swimmer_data, aes(y = reorder(USUBJID, subject_order))) +
  geom_segment(
    aes(x = 0, xend = TRTDURD, yend = reorder(USUBJID, subject_order), color = ARM),
    linewidth = 3
  ) +
  geom_text(
    aes(x = TRTDURD, label = EOSSTT),
    hjust = -0.1,
    size = 3,
  ) +
  labs(
    title = "Swimmer Plot with End of Study Status",
    x = "Days Since Treatment Start",
    y = "Subject ID",
    color = "Treatment Arm"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    legend.position = "top"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))
