#' WCM R markdown template
#'
#' Template for creating an R markdown document for WCM Biostatistics reports
#'
#' @inheritParams rmarkdown::html_document
#' @export
wcm <- function(toc = FALSE,
                toc_depth = 3,
                toc_float = FALSE,
                number_sections = FALSE,
                section_divs = TRUE,
                fig_width = 7,
                fig_height = 5,
                fig_retina = 2,
                fig_caption = TRUE,
                dev = 'png',
                df_print = "default",
                code_folding = c("none", "show", "hide"),
                code_download = FALSE,
                smart = TRUE,
                self_contained = TRUE,
                theme = "default",
                highlight = "default",
                mathjax = "default",
                template = "default",
                extra_dependencies = NULL,
                css = NULL,
                includes = NULL,
                keep_md = FALSE,
                lib_dir = NULL,
                md_extensions = NULL,
                pandoc_args = NULL,
                ...
) {

  # build pandoc args
  args <- c("--standalone")

  # use section divs
  if (section_divs)
    args <- c(args, "--section-divs")

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  md_extensions <- rmarkdown:::smart_extension(smart, md_extensions)

  # toc_float
  if (toc && !identical(toc_float, FALSE)) {

    # must have a theme
    if (is.null(theme))
      stop("You must use a theme when specifying the 'toc_float' option")

    # resolve options
    toc_float_options <- list(collapsed = TRUE,
                              smooth_scroll = TRUE,
                              print = TRUE)
    if (is.list(toc_float)) {
      toc_float_options <- merge_lists(toc_float_options, toc_float)
      toc_float <- TRUE
    } else if (!isTRUE(toc_float)) {
      stop("toc_float must be a logical or a list with options")
    }

    # dependencies
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_jquery(),
                                      html_dependency_jqueryui(),
                                      html_dependency_tocify()))

    # flag for template
    args <- c(args, pandoc_variable_arg("toc_float", "1"))

    # selectors
    selectors <- paste0("h", seq(1, toc_depth), collapse = ",")
    args <- c(args, pandoc_variable_arg("toc_selectors", selectors))

    # options
    if (toc_float_options$collapsed)
      args <- c(args, pandoc_variable_arg("toc_collapsed", "1"))
    if (toc_float_options$smooth_scroll)
      args <- c(args, pandoc_variable_arg("toc_smooth_scroll", "1"))
    if (toc_float_options$print)
      args <- c(args, pandoc_variable_arg("toc_print", "1"))
  }

  # template path and assets
  if (identical(template, "default"))
    args <- c(args, "--template",
              pandoc_path_arg(system.file("rmarkdown", "templates", "wcm", "template.html", package = "wcmtheme")))
  else if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))

  # validate code_folding
  code_folding <- match.arg(code_folding)

  # navigation dependencies
  if (!is.null(theme)) {
    code_menu <- !identical(code_folding, "none") || code_download
    source_embed <- code_download
    extra_dependencies <- append(extra_dependencies,
                                 list(
                                   html_dependency_jquery(),
                                   rmarkdown:::html_dependency_navigation(code_menu = code_menu,
                                                              source_embed = source_embed)
                                 )
    )
  }

  # highlight
  args <- c(args, rmarkdown:::pandoc_html_highlight_args(template, highlight))

  # add highlight.js html_dependency if required
  if (identical(template, "default") && rmarkdown:::is_highlightjs(highlight)) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_highlightjs(highlight)))
  }

  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")

  # additional css
  base_css <- system.file("rmarkdown", "templates", "wcm", "resources", "wcm.css", package = "wcmtheme")
  args <- c(args, "--css", pandoc_path_arg(base_css))

  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file))

  # manage list of exit_actions (backing out changes to knitr options)
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions)
      try(action())
  }

  # capture the source code if requested
  source_code <- NULL
  source_file <- NULL
  pre_knit <- function(input, ...) {
    if (code_download) {
      source_file <<- basename(input)
      source_code <<- paste0(
        '<div id="rmd-source-code">',
        base64enc::base64encode(input),
        '</div>')
    }
  }

  # pagedtable
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies,
                                 list(html_dependency_pagedtable()))
  }

  # pre-processor for arguments that may depend on the name of the
  # the input file AND which need to inject html dependencies
  # (otherwise we could just call the pre_processor)
  post_knit <- function(metadata, input_file, runtime, encoding, ...) {

    # extra args
    args <- c()

    # navbar (requires theme)
    if (!is.null(theme)) {

      # add navbar to includes if necessary
      navbar <- file.path(rmarkdown:::normalize_path(dirname(input_file)), "_navbar.html")

      # if there is no _navbar.html look for a _navbar.yml
      if (!file.exists(navbar)) {
        navbar_yaml <- file.path(dirname(navbar), "_navbar.yml")
        if (file.exists(navbar_yaml))
          navbar <- navbar_html_from_yaml(navbar_yaml)
        # if there is no _navbar.yml then look in site config (if we have it)
        config <- site_config(input_file, encoding)
        if (!is.null(config) && !is.null(config$navbar))
          navbar <- navbar_html(config$navbar)
      }

      if (file.exists(navbar)) {

        # include the navbar html
        includes <- list(before_body = navbar)
        args <- c(args, includes_to_pandoc_args(includes,
                                                filter = if (is_shiny_classic(runtime))
                                                  function(x) rmarkdown:::normalize_path(x, mustWork = FALSE)
                                                else
                                                  identity))

        # flag indicating we need extra navbar css and js
        args <- c(args, pandoc_variable_arg("navbar", "1"))
        # variables controlling padding from navbar
        args <- c(args, pandoc_body_padding_variable_args(theme))

        # navbar icon dependencies
        iconDeps <- navbar_icon_dependencies(navbar)
        if (length(iconDeps) > 0)
          knitr::knit_meta_add(list(iconDeps))
      }
    }

    args
  }

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # track whether we have a code menu
    code_menu <- FALSE

    # code_folding
    if (code_folding %in% c("show", "hide")) {
      # must have a theme
      if (is.null(theme))
        stop("You must use a theme when specifying the 'code_folding' option")
      args <- c(args, pandoc_variable_arg("code_folding", code_folding))
      code_menu <- TRUE
    }

    # source_embed
    if (code_download) {
      if (is.null(theme))
        stop("You must use a theme when specifying the 'code_download' option")
      args <- c(args, pandoc_variable_arg("source_embed", source_file))
      sourceCodeFile <- tempfile(fileext = ".html")
      write_utf8(source_code, sourceCodeFile)
      args <- c(args, pandoc_include_args(after_body = sourceCodeFile))
      code_menu <- TRUE
    }

    # code menu
    if (code_menu)
      args <- c(args, pandoc_variable_arg("code_menu", "1"))

    # content includes (we do this here so that user include-in-header content
    # goes after dependency generated content). make the paths absolute if
    # making a Shiny document so we can resolve them even if rendering
    # elsewhere.
    args <- c(args, includes_to_pandoc_args(includes,
                                            filter = if (is_shiny_classic(runtime))
                                              function(x) rmarkdown:::normalize_path(x, mustWork = FALSE)
                                            else
                                              identity))

    # return additional args
    args
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev),
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(fig_caption, md_extensions),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    df_print = df_print,
    pre_knit = pre_knit,
    post_knit = post_knit,
    pre_processor = pre_processor,
    on_exit = on_exit,
    base_format = html_document_base(smart = smart, theme = theme,
                                     self_contained = self_contained,
                                     lib_dir = lib_dir, mathjax = mathjax,
                                     template = template,
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     ...)
  )
}
