library(shinylive)

shinylive::export(appdir ="shinylive_test", output_dir="site")
httpuv::runStaticServer("site")