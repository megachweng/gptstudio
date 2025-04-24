#' ChatGPT in Source
#'
#' Call this function as a Rstudio addin to ask GPT to improve spelling and
#' grammar of selected text.
#'
#' @export
#'
#' @return This function has no return value.
#'
#' @examples
#' # Select some text in a source file
#' # Then call the function as an RStudio addin
#' \dontrun{
#' gptstudio_chat_in_source()
#' }
gptstudio_chat_in_source_addin <- function() {
  gptstudio_chat_in_source_dialog()
}

gptstudio_chat_in_source_dialog <- function() {
  library(shiny)
  library(miniUI)
  library(clipr)
  library(glue)

  ui <- miniPage(
    tags$head(
      tags$style(HTML("
        .mini-content { height: calc(100vh - 80px) !important; display: flex; flex-direction: column;}
        .main-area { flex: 1 1 auto; }
        .footer-bar {
          display: flex;
          align-items: center;
          padding: 0 12px 12px 12px;
          background: #fff;
          gap: 12px;
        }
        .footer-bar .form-group {
          margin-bottom: 0;
          width: 100%;
        }
        .footer-bar input.form-control {
          height: 40px !important;
          font-size: 18px;
          padding: 6px 12px;
          border-radius: 8px;
          margin: 0;
        }
        .footer-bar .btn {
          height: 40px;
          padding: 6px 26px 6px 26px;
          font-size: 18px;
          border-radius: 8px;
          line-height: 1;
          margin: 0;
          min-width: 98px;
        }
        #result_text, #selection_text {
          width: 100%;
          height: 250px;
          resize: vertical;
          border: 1px solid #6c757d;
          border-radius: 6px;
          padding: 10px;
          font-size: 16px;
          margin-bottom: 15px;
          font-family: 'Fira Mono', 'Consolas', 'Menlo', 'Monaco', monospace;
          background-color: #f6f8fa;
          color: #24292f;
          overflow-x: auto;
          white-space: pre;
        }
        #selection_text {
          height: 100px;
          margin-bottom: 10px;
        }
      ")),
      tags$script(HTML('
        // 让窗口打开时聚焦输入框
        $(document).on("shiny:connected", function() {
          setTimeout(function(){
            $("#prompt").focus();
          }, 100);
        });
        // 支持 Ctrl+Enter/⌘+Enter 发送
        $(document).on("keydown", "#prompt", function(e) {
          if ((e.key === "Enter" && !e.shiftKey && !e.altKey) && (e.ctrlKey || e.metaKey)) {
            e.preventDefault();
            $("#send").click();
          }
        });
        // 响应R发来的新内容，填入textarea
        Shiny.addCustomMessageHandler("updateResultText", function(msg){
          document.getElementById("result_text").value = msg.value;
        });
        // 响应R发来的选中文本，填入selection_text
        Shiny.addCustomMessageHandler("updateSelectionText", function(msg){
          document.getElementById("selection_text").value = msg.value;
        });
      '))
    ),
    gadgetTitleBar("AI助手"),
    miniContentPanel(
      div(
        class = "mini-content",
        div(
          style = "margin-bottom: 10px;",
          tags$label("选中文本：", `for` = "selection_text", style = "font-weight: bold;"),
          tags$textarea(
            id = "selection_text",
            placeholder = "RStudio中选中的文本会显示在这里",
            readonly = "readonly",
            style = "width: 100%; height: 100px; resize: vertical; margin-bottom: 10px;"
          )
        ),
        div(
          style = "display: flex; justify-content: flex-end; margin: 8px 0;",
          actionButton("copy", label = tagList(shiny::icon("copy"), "Copy")),
          actionButton("insert", label = tagList(shiny::icon("plus"), "Insert"), style = "margin-left: 8px;")
        ),
        div(
          class = "main-area",
          tags$textarea(
            id = "result_text",
            placeholder = "AI返回的结果这里显示",
            readonly = "readonly"
          )
        ),
        div(
          class = "footer-bar",
          div(style = "flex: 1 1 auto;", textInput("prompt", NULL, placeholder = "Prompt", width = "100%")),
          actionButton("send", "Send")
        )
      )
    )
  )

  server <- function(input, output, session) {
    rv <- reactiveValues(response = "")
    
    observeEvent(input$send, {
      req(input$prompt)
      service <- getOption("gptstudio.service")
      model <- getOption("gptstudio.model")
      selection <- get_selection()
      instructions <- create_generic_task(selection, input$prompt)
  
      cli::cli_progress_step(msg = "Sending query to {service}...", spinner = TRUE)
      cli::cli_progress_update()

      response <-
        gptstudio_create_skeleton(
          service = service,
          prompt  = instructions,
          history = list(),
          stream  = FALSE,
          model   = model
        ) |>
        gptstudio_request_perform()

      rv$response <- as.character(response$response)
      cli_process_done()
      # 通过自定义消息把内容塞到 textarea
      session$sendCustomMessage("updateResultText", list(value = rv$response))
    })

    # 确保UI打开时 textarea 内容也能绑定
    observe({
      session$sendCustomMessage("updateResultText", list(value = rv$response))
    })
    observe({
      session$sendCustomMessage("updateSelectionText", list(value = get_selection()))
    })
    observeEvent(input$copy, {
      req(rv$response)
      clipr::write_clip(rv$response)
      showNotification("已复制到剪贴板", type = "message")
    })

    observeEvent(input$insert, {
      req(rv$response)
      insert_text(rv$response)
      showNotification("已插入到编辑器", type = "message")
    })

    observeEvent(input$done, {
      stopApp(invisible())
    })

    observeEvent(input$cancel, {
      stopApp(invisible())
    })

    session$onSessionEnded(function() {
      stopApp(invisible())
    })
  }

  tryCatch(
    {
      runGadget(ui, server, viewer = dialogViewer("AI助手"))
    },
    error = function(e) {
      if (inherits(e, "shiny.silent.error")) {
        return(invisible())
      }
      stop(e)
    },
    finally = {
      if (exists(".shiny_last_app", envir = .GlobalEnv)) {
        rm(".shiny_last_app", envir = .GlobalEnv)
      }
    }
  )
}

create_generic_task <- function(selection, instructions) {
  file_ext <- get_file_extension()
  glue::glue(
    "You are an expert on following instructions without making conversation.\n",
    "Your response will go directly into an open .{file_ext} file in RStudio IDE.\n",
    "Output only plain text. Do not output markdown.\n",
    "Selection:\n{selection}\n",
    "Instructions:\n{instructions}\n"
  )
}

get_file_extension <- function() {
  file_ext <- character(1L)
  tryCatch(expr = {
    doc_path <- rstudioapi::documentPath()
    file_ext <<- tools::file_ext(doc_path)
  }, error = function(e) {
    cli::cli_alert_warning("Current document is not saved. Assuming .R file extension")
    file_ext <<- "R"
  })
  return(file_ext)
}
