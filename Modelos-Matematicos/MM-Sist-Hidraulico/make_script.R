library(ymlthis)
library(here)
library(fs)

# Manejo de Directorios y Archivos.
hw_dir <- dir_create(here("MM-Sist-Hidraulico"))
fig_dir <- dir_create(here("MM-Sist-Hidraulico", "fig"))
content_dir <- dir_create(here("MM-Sist-Hidraulico", "content"))
tex_dir <- dir_create(here("MM-Sist-Hidraulico", "tex"))

# Preambulo para latex document:
tex_file <- file_create(path(tex_dir, "preamble", ext="tex"))
lines <- c("\\renewcommand{\\baselinestretch}{1.2}", "\\usepackage{amsmath}", "\\usepackage{indentfirst}")
writeLines(lines, tex_file)

# Construccion del encabezado YAML
hw_yaml <- yml_empty() %>%
  yml_author("Marcelo J. Molinatti S.") %>%
  yml_date(format(Sys.Date(), "%B %d, %Y")) %>%
  yml_title("Sistema Hidráulico equivalente al circuito RLC.") %>%
  yml_subtitle("Modelo matemático y solución por transformada de Laplace") %>%
  yml_output(
    bookdown::pdf_document2(
      number_sections=TRUE, 
      toc = FALSE,
      keep_tex=TRUE, dev="png",
      includes=rmarkdown::includes(in_header="./tex/preamble.tex"))
    ) %>%
  yml_latex_opts(
    geometry=c("left=3cm", "right=3cm", "top=2.5cm", "bottom=2.5cm"), 
    linestretch=1.5, 
    secnumdepth=2, 
    colorlinks=TRUE
  ) %>%
  yml_citations(bibliography = path(tex_dir, "references", ext="bib")) %>%
  yml_lang("es-ES")

# Verificando sobreesctrura
if (file_exists(path(hw_dir, "hw-MM-Sist-Hidraulico", ext="Rmd")))
  file_delete(path(hw_dir, "hw-MM-Sist-Hidraulico", ext="Rmd"))

# Creacion del archivo Rmarkdown
hw_yaml %>%
  use_rmarkdown(
    path=path(hw_dir, "hw-MM-Sist-Hidraulico", ext="Rmd"),
    body=code_chunk({
      library(RefManageR)
    }, chunk_args=list(include=FALSE, message=FALSE, warning=FALSE))
  )

# Appending content
hw_content <- dir_ls(content_dir)
hw_content <- hw_content[stringr::str_detect(hw_content, "\\d{2}")]
file.append(
  path(hw_dir, "hw-MM-Sist-Hidraulico", ext="Rmd"),
  hw_content
)

# Creacion de Bibtex file
bib_file <- file_create(path(tex_dir, "references", ext="bib"))
lines <- c(
  "@book{ogata1987dinamica,
    title={Din{\\'a}mica de sistemas},
    author={Ogata, Katsuhiko and Sanchez, Guillermo Lopez Portillo},
    volume={3},
    year={1987},
    publisher={Prentice-Hall Hispanoamericana}
  }",
  "@book{zill2008ecuaciones,
    title={Ecuaciones diferenciales},
    author={Zill, Dennis G and Cullen, Michael R},
    year={2008},
    publisher={McGraw-Hill}
  }"
)
writeLines(lines, bib_file)

# Renderizando
rmarkdown::render(here("MM-Sist-Hidraulico", "hw-MM-Sist-Hidraulico.Rmd"))