#' Run GPTStudio Chat Gadget
#'
#' 启动 Gadget 方式的 ChatGPT 界面，支持插入代码到 RStudio。
#' @export
gptstudio_chat_gadget <- function() {
  ui <- gptstudio::mod_app_ui("app")
  server <- function(input, output, session) {
    gptstudio::mod_app_server("app")
  }
  shiny::runGadget(ui, server)
}