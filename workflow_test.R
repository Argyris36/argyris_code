library("workflowr")
wflow_start("~/argyris_code", existing = TRUE)
wflow_build()
wflow_view()
wflow_status()
wflow_publish(c("analysis/index.Rmd", "analysis/about.Rmd", "analysis/license.Rmd"),
              "Publish the initial files for myproject")
wflow_use_github("Argyris36")
wflow_git_push(dry_run = TRUE)
wflow_open("analysis/first-analysis.Rmd")
wflow_build()
wflow_status()
wflow_publish(c("analysis/index.Rmd", "analysis/testing_CV.Rmd"),
              "Add my first analysis")
wflow_build()
wflow_git_push()

#git push -u origin main  # do this via terminal


setwd("/Users/stringarisa/my_project/my_project")
wflow_start("my_project", git = TRUE, existing= TRUE)
wflow_publish(c("analysis/index.Rmd", "analysis/testing_CV.Rmd"),
              "Add my first analysis")
wflow_build()
