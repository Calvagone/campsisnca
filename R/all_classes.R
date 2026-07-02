setOldClass("tbl_df")
setOldClass(c("std_campsis_tbl", "tbl_df", "data.frame"))
setClassUnion("campsis_output", c("data.frame", "tbl_df", "std_campsis_tbl"))
