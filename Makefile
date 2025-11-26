.PHONY: init lock clean test run exp report document

RSCRIPT = Rscript --vanilla
RENV_INIT = "source('scripts/renv_init.R'); "

init:
	$(RSCRIPT) scripts/00_bootstrap.R || R --vanilla -q -f scripts/00_bootstrap.R

test:
	$(RSCRIPT) -e $(RENV_INIT)"devtools::test()"

run:
	$(RSCRIPT) -e $(RENV_INIT)"targets::tar_make()"

exp:
	$(RSCRIPT) -e $(RENV_INIT)"targets::tar_make()"

report:
	$(RSCRIPT) -e $(RENV_INIT)"quarto::quarto_render('reports/paper.qmd')"

document:
	$(RSCRIPT) -e $(RENV_INIT)"devtools::document()"

lock:
	Rscript --vanilla -e "renv::snapshot(prompt = FALSE)" || R --vanilla -q -e "renv::snapshot(prompt = FALSE)"

clean:
	rm -rf _targets/
