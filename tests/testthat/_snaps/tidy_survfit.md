# tidy_survfit() messaging

    Code
      invisible(tidy_survfit(survfit(Surv(time - 500, status) ~ 1, df_lung)))
    Message
      ! Setting start time to -499.835728952772.
      i Specify `ggsurvfit::survfit2(start.time)` to override this default.

