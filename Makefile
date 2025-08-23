.PHONY: init lock clean

init:
	Rscript scripts/00_bootstrap.R

lock:
	Rscript -e "renv::snapshot(prompt = FALSE)"

clean:
	rm -rf _targets/

