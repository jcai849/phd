op_time <- function(op) function(x, m) seq(x, by=op, length.out=m+1)[m+1]
add_month <- op_time("month"); subtract_month <- op_time("-1 month")

(enrolment <- as.Date("2020-05-01"))
(earliest_submission <- add_month(enrolment, 36)) # (1)
(latest_submission <- add_month(enrolment, 48) - 1)
(ideal_submission <- subtract_month(as.Date("2023-07-01"), 4))
(submission <- earliest_submission)
(intention_to_submit <- subtract_month(submission, 3)) # (2)
(recommended_time_to_find_examiners_in_order_to_minimise_procedure_time_with_SGS <- Sys.Date()) # (3)

# (0) Outline of process: https://www.auckland.ac.nz/en/students/academic-information/postgraduate-students/doctoral/thesis-and-examination/doctoral-examination-process.html
# (1) S Timing of submission SS 3: https://www.auckland.ac.nz/en/about-us/about-the-university/policy-hub/research-innovation/doctoral-study/examination/doctoral-thesis-submission-pre-examination.html
# (2) https://uoa.custhelp.com/app/answers/detail/a_id/1949
# (3) Lois Xu has just done final submission, strongly recommended this
