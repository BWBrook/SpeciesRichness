.PHONY: init lock clean

init:
	Rscript --vanilla scripts/00_bootstrap.R || R --vanilla -q -f scripts/00_bootstrap.R

lock:
	Rscript --vanilla -e "renv::snapshot(prompt = FALSE)" || R --vanilla -q -e "renv::snapshot(prompt = FALSE)"

clean:
	rm -rf _targets/
