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
        .main-area { flex: 1 1 auto; margin-bottom: 15px; } /* Added margin here */
        .footer-bar {
          display: flex;
          align-items: center;
          padding: 0 12px 12px 12px;
          background: #fff;
          gap: 12px;
          margin-top: 10px; /* Added margin here */
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
          height: 150px; /* 初始状态较高 */
          margin-bottom: 10px;
          transition: height 0.3s ease;
        }
        /* 结果区域初始隐藏 */
        .result-area {
          display: none;
        }
        /* Improved code-wrapper styling */
        .code-wrapper {
          position: relative;
          width: 100%;
          min-height: 100px; /* Minimum height */
          overflow: auto;
          border: 1px solid #6c757d;
          border-radius: 6px;
          padding: 10px;
          font-size: 16px;
          background-color: #f6f8fa;
          margin-bottom: 15px;
          display: block; /* Ensure proper block display */
        }
        /* Result area specific */
        .main-area .code-wrapper {
          min-height: 250px;
        }
        .code-wrapper pre {
          margin: 0;
          border: none;
          background: transparent;
          height: 100%;
          padding: 0;
          overflow: auto;
        }
        .hljs {
          background: transparent !important;
          padding: 0 !important;
          height: 100%;
          white-space: pre;
          font-family: 'Fira Mono', 'Consolas', 'Menlo', 'Monaco', monospace;
        }
      ")),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/vs.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/r.min.js"),
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
        
        // 显示结果区域并调整选中文本框高度的函数
        function showResultArea() {
          document.querySelector(".result-area").style.display = "block";
          document.getElementById("selection_text").style.height = "80px"; // 缩小选中文本区域
          
          // 应用过渡动画效果
          setTimeout(function() {
            const selectionWrapper = document.querySelector("#selection_text").parentElement.querySelector(".code-wrapper");
            if (selectionWrapper) {
              selectionWrapper.style.minHeight = "80px";
              selectionWrapper.style.height = "80px";
            }
          }, 0);
        }
        
        // 代码高亮处理函数
        function applyHighlighting(elementId, text, language) {
          // 获取对应元素
          const element = document.getElementById(elementId);
          if (!element) return;
          
          // 获取元素高度以保留
          const originalHeight = element.offsetHeight;
          
          // 先移除旧的高亮容器
          const parent = element.parentElement;
          const existingWrapper = parent.querySelector(".code-wrapper");
          if (existingWrapper) {
            existingWrapper.remove();
          }
          
          // 隐藏原始textarea但保留其功能
          element.style.position = "absolute";
          element.style.left = "-9999px";
          element.style.height = "1px"; // Keep minimal height instead of 100%
          element.style.opacity = "0";
          element.value = text;
          
          // 创建高亮显示容器
          const wrapper = document.createElement("div");
          wrapper.className = "code-wrapper";
          if (elementId === "result_text") {
            wrapper.style.minHeight = Math.max(250, originalHeight) + "px";
          } else {
            wrapper.style.minHeight = Math.max(100, originalHeight) + "px";
          }
          
          const pre = document.createElement("pre");
          const code = document.createElement("code");
          if (language) {
            code.className = "language-" + language;
          }
          code.textContent = text;
          pre.appendChild(code);
          wrapper.appendChild(pre);
          parent.appendChild(wrapper);
          
          // 应用高亮
          hljs.highlightElement(code);
        }
        
        // 响应R发来的新内容，填入textarea并应用高亮
        Shiny.addCustomMessageHandler("updateResultText", function(msg){
          document.getElementById("result_text").value = msg.value;
          applyHighlighting("result_text", msg.value, Shiny.getFileExtension || "r");
          // 只有当有内容时才显示结果区域
          if (msg.value && msg.value.trim().length > 0) {
            showResultArea();
          }
        });
        
        // 响应R发来的选中文本，填入selection_text并应用高亮
        Shiny.addCustomMessageHandler("updateSelectionText", function(msg){
          document.getElementById("selection_text").value = msg.value;
          applyHighlighting("selection_text", msg.value, Shiny.getFileExtension || "r");
        });
        
        // 存储文件扩展名以供高亮使用
        Shiny.addCustomMessageHandler("setFileExtension", function(msg){
          Shiny.getFileExtension = msg.value.toLowerCase();
        });
      '))
    ),
    gadgetTitleBar("AI Assistant"),
    miniContentPanel(
      div(
        class = "mini-content",
        div(
          style = "margin-bottom: 10px;",
          tags$label("Selected Text:", `for` = "selection_text", style = "font-weight: bold;"),
          tags$textarea(
            id = "selection_text",
            placeholder = "Text selected in RStudio will be shown here",
            readonly = "readonly",
            style = "width: 100%; height: 150px; resize: vertical; margin-bottom: 10px;"
          )
        ),
        div(
          class = "result-area", # 添加result-area类用于控制显示/隐藏
          div(
            style = "display: flex; justify-content: flex-end; margin: 8px 0;",
            actionButton("copy", label = tagList(shiny::icon("copy"), "Copy")),
            actionButton("insert", label = tagList(shiny::icon("plus"), "Insert"), style = "margin-left: 8px;")
          ),
          div(
            class = "main-area",
            tags$textarea(
              id = "result_text",
              placeholder = "AI response will be displayed here",
              readonly = "readonly"
            )
          )
        ),
        div(
          class = "footer-bar",
          div(style = "flex: 1 1 auto;", textInput("prompt", NULL, placeholder = "Ask AI", width = "100%")),
          actionButton("send",label = tagList(shiny::icon("paper-plane"), "Send")),
        )
      )
    )
  )

  server <- function(input, output, session) {
    rv <- reactiveValues(response = "")
    
    #  Send file extension to client for syntax highlighting
    observe({
      file_ext <- get_file_extension()
      session$sendCustomMessage("setFileExtension", list(value = file_ext))
    })
    
    observeEvent(input$send, {
      req(input$prompt)
      service <- getOption("gptstudio.service")
      model <- getOption("gptstudio.model")
      selection <- get_selection()
      instructions <- create_generic_task2(selection, input$prompt)
  
      cli::cli_progress_step(msg = "Sending query to AI...", spinner = TRUE)
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
      # Send content to textarea through custom message
      session$sendCustomMessage("updateResultText", list(value = rv$response))
    })

    # 确保UI打开时 textarea 内容也能绑定
    observe({
      if (!is.null(rv$response) && nchar(rv$response) > 0) {
        session$sendCustomMessage("updateResultText", list(value = rv$response))
      }
    })
    observe({
      session$sendCustomMessage("updateSelectionText", list(value = get_selection()))
    })
    observeEvent(input$copy, {
      req(rv$response)
      clipr::write_clip(rv$response)
      showNotification("Copied to clipboard", type = "message")
    })

    observeEvent(input$insert, {
      req(rv$response)
      insert_text(rv$response)
      showNotification("Inserted into editor", type = "message")
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
      runGadget(ui, server, viewer = dialogViewer(dialogName = "AI Assistant", width = 800, height = 600))
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

gptstudio_chat_in_source <- function(task = NULL, keep_selection = TRUE) {
  selection <- get_selection()
  service <- getOption("gptstudio.service")
  model <- getOption("gptstudio.model")
  task <- task %||% create_generic_task()

  instructions <- glue::glue("{task}: {selection}")

  cli::cli_progress_step(
    msg = "Sending query to AI...",
    msg_done = "{service} responded",
    spinner = TRUE
  )

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

  text_to_insert <- as.character(response$response)

  if (keep_selection) {
    text_to_insert <- c(selection, text_to_insert)
  }

  cli_process_done()

  insert_text(text_to_insert)
}

create_generic_task <- function() {
  file_ext <- get_file_extension()

  glue::glue(
    "You are an expert on following instructions without making conversation.",
    "Do the task specified after the colon.",
    "Your response will go directly into an open .{file_ext} file in an IDE",
    "without any post processing.",
    "Output only plain text. Do not output markdown.",
    .sep = " "
  )
}

create_generic_task2 <- function(selection, instructions) {
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
