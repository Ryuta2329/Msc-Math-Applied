library(ymlthis)
library(here)
library(fs)
library(RefManageR)
library(raster)

# Manejo de Directorios y Archivos.
seminary <- dir_create(here("Seminario-Mod-Math"))
fig_dir <- dir_create(here("Seminario-Mod-Math", "fig"))
content_dir <- dir_create(here("Seminario-Mod-Math", "content"))

# Construccion del encabezado YAML
yml_seminario <- yml_empty() %>%
  yml_author("Marcelo J. Molinatti S.") %>%
  yml_date(format(Sys.Date(), "%B %d, %Y")) %>%
  yml_title("Método de Extracción de Masas de Agua basado en datos satelitales del Landsat-8") %>%
  yml_subtitle("Aplicado al lago de Valencia, Carabobo.") %>%
  yml_output(
  	xaringan::moon_reader(
      lib_dir = "libs",
      css = list("xaringan-themer.css", "slide-theme.css"),
      seal = FALSE,
      nature = list(
        highlightStyle = "github",
        highlightLines = TRUE,
        countIncrementalSlides = FALSE
  ))) %>%
  yml_lang("es-ES")

# Verificando sobreesctrura
if (file_exists(path("Seminario-Mod-Math", "Seminario-Mod-Math", ext="Rmd")))
  file_delete(path("Seminario-Mod-Math", "Seminario-Mod-Math", ext="Rmd"))

# Creacion del archivo Rmarkdown
yml_seminario %>%
  use_rmarkdown(
    path=path("Seminario-Mod-Math", "Seminario-Mod-Math", ext="Rmd"),
    body=code_chunk(
      {
        library(kableExtra)

        options(
          knitr.table.format="latex", 
          kableExtra.latex.load_packages=TRUE, 
          knitr.kable.NA = "")

        library(xaringanthemer)
        library(xaringanExtra)

    		# https://colorhunt.co/palette/343434e6b31efcfaf1cacaca
        style_duo(primary_color = "#FCFAF1", secondary_color = "#343434")
    		use_animate_css()
      },
      chunk_args=list(include=FALSE, message=FALSE, warning=FALSE)
  	)
  )

# Appending content
content <- dir_ls(path("Seminario-Mod-Math", "content"))
file.append(
  path("Seminario-Mod-Math", "Seminario-Mod-Math", ext="Rmd"), 
  content
) 

# Referencias
bib <- BibEntry(
  bibtype = "phdthesis", 
  key = "mauricio2023", 
  title = "Método de Extracción de Masas de Agua basado en datos satelitales del Landsat-8: Aplicado al lago de Valencia, Carabobo.",
  school = "Universidad de Carabobo", 
  year = 2023, 
  author = "Mauricio Davil", 
  adress = "Valencia, Carabobo")

# Generando raster file
# https://michaelminn.net/tutorials/r-landsat/index.html

# Renderizando
rmarkdown::render(path("Seminario-Mod-Math", "Seminario-Mod-Math", ext="Rmd"))
to_pdf(from = fs::path("Seminario-Mod-Math", "Seminario-Mod-Math", ext="Rmd"))