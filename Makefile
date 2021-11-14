.PHONY: clean
clean:
	rm -rf notes data

.PHONY: generate
generate: clean
	eldev -C --unstable -a -dtT exec '(generate-data "./")'

.PHONY: db
db:
	eldev -C --unstable -a -dtT exec '(sync-db "./")'
